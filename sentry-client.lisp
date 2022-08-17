(in-package #:sentry-client)

(defvar *sentry-client*
  "The current sentry client.")

(defparameter +dsn-regex+ "(.*)\\:\\/\\/(.*)\\@(.*)\\/(.*)")

(defun parse-dsn (dsn-string)
  "Parse a DSN string to a list object.
See: https://docs.sentry.io/product/sentry-basics/dsn-explainer/"

  ;; Format: {PROTOCOL}://{PUBLIC_KEY}@{HOST}/{PATH}{PROJECT_ID}
  (ppcre:register-groups-bind (protocol public-key host project-id)
      (+dsn-regex+ dsn-string)
    (when (not (every (alexandria:compose 'not 'null)
                      (list protocol public-key host project-id)))
      (error "Bad DSN format"))
    (return-from parse-dsn
      (list :protocol protocol
            :public-key public-key
            :host host
            :project-id project-id
            :uri (concatenate 'string protocol "://" host))))
  (error "Bad DSN format"))

(defun read-dsn (dsn)
  "Return a DSN object. If DSN is an object already, it returns it. If it is a string, it parses it."
  (cond
    ((stringp dsn) (parse-dsn dsn))
    ((consp dsn) dsn)
    (t (error "Invalid DSN: ~A" dsn))))

(defclass sentry-client ()
  ((dsn :initarg :dsn
        :initform (error "Provide the DSN")
        :accessor dsn)
   (transport :initarg :transport
              :initform :http
              :accessor sentry-transport)
   (connection-timeout :initarg :connection-timeout
                       :initform 10
                       :accessor connection-timeout))
  (:documentation "A sentry client"))

(defmethod initialize-instance :after ((sentry-client sentry-client) &rest initargs)
  (declare (ignore initargs))
  (setf (dsn sentry-client) (read-dsn (dsn sentry-client))))

(defun make-sentry-client (dsn &optional (class 'sentry-client))
  (make-instance class :dsn (read-dsn dsn)))

(defun initialize-sentry-client (dsn &rest args &key (client-class 'sentry-client))
  (setf *sentry-client*
        (apply #'make-instance client-class
               :dsn (read-dsn dsn)
               (alexandria:remove-from-plist args :client-class))))

(defun call-with-sentry-client (function dsn &rest args)
  "Call FUNCTION in the context of a SENTRY-CLIENT instantied using DSN and ARGS."
  (let ((*sentry-client* (apply #'make-sentry-client dsn args)))
    (funcall function)))

(defmacro with-sentry-client (() &body body)
  `(call-with-sentry-client (lambda () ,@body) ,dsn ,@args))

(defun sentry-api-url (&optional (sentry-client *sentry-client*))
  (concatenate 'string (getf (dsn sentry-client) :uri) "/api/"
               (getf (dsn sentry-client) :project-id) "/store/"))

(defparameter +sentry-timestamp-format+
  (append local-time:+iso-8601-date-format+
          (list #\T) local-time:+iso-8601-time-format+))

(defun format-sentry-timestamp (stream timestamp)
  "Format TIMESTAMP for Sentry."
  (local-time:format-timestring stream
                                (local-time:now)
                                :format +sentry-timestamp-format+
                                :timezone local-time:+UTC-ZONE+))

(defun encode-sentry-auth-header (sentry-client)
  "Encode Sentry authentication header."
  (fmt:fmt nil "Sentry sentry_version=" 5 ","
           "sentry_client=" "cl-sentry-client/"
           (asdf:component-version (asdf:find-system :sentry-client)) ","
           "sentry_timestamp=" (format-sentry-timestamp nil (local-time:now)) ","
           "sentry_key=" (getf (dsn sentry-client) :public-key) ","
           "sentry_secret=" (getf (dsn sentry-client) :secret-key)))

(defun post-sentry-request (data &optional (sentry-client *sentry-client*))
  "Just send DATA to sentry api via HTTP."
  (drakma:http-request
   (sentry-api-url)
   :method :post
   :content-type "application/json"
   :content data
   :additional-headers
   (list
    (cons "X-Sentry-Auth" (encode-sentry-auth-header sentry-client)))
   :connection-timeout (connection-timeout sentry-client)))

(defun make-sentry-event-id ()
  "Create an ID for a new Sentry event."
  (ironclad:byte-array-to-hex-string
   (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))

(defun encode-core-event-attributes (json-stream &key extras sentry-client)
  (declare (ignorable sentry-client))
  (json:encode-object-member "event_id" (make-sentry-event-id) json-stream)
  (json:encode-object-member "timestamp" (format-sentry-timestamp nil (local-time:now)) json-stream)
  (json:encode-object-member "logger" "cl-sentry-client" json-stream)
  (json:encode-object-member "platform" "other" json-stream)
  (json:as-object-member ("extra" json-stream)
    (json:with-object (json-stream)
      (loop for extra in extras
            collect
            (destructuring-bind (key . val) extra
              (json:encode-object-member (princ-to-string key) (princ-to-string val))))))
  (json:as-object-member ("sdk" json-stream)
    (json:with-object (json-stream)
      (json:encode-object-member "name" "cl-sentry-client")
      (json:encode-object-member "version" (asdf:component-version (asdf:find-system :sentry-client))))))

(defun encode-exception (condition json-stream &optional (sentry-client *sentry-client*))
  (json:encode-object-member "type" (princ-to-string (type-of condition)) json-stream)
  (json:encode-object-member "value" (princ-to-string condition) json-stream)
  (json:encode-object-member "module" (princ-to-string (package-name (symbol-package (type-of condition)))) json-stream)
  (json:as-object-member ("stacktrace")
    (encode-stacktrace condition json-stream sentry-client)))

#+lispworks
(defun lw-map-backtrace (fn)
  "On Lispworks, trivial-backtrace:map-backtrace is unavailable. Maybe
move this to trivial-backtrace in the future"
  (mp:map-process-backtrace
   mp:*current-process*
   (lambda (frame)
     (funcall
      fn
      (trivial-backtrace::make-frame
       :func frame
       :source-filename (let* ((locations (dspec:find-dspec-locations frame))
                               (loc (cadr (car locations))))
                          (if (symbolp loc)
                              loc
                              (namestring loc)))
       :source-pos "unknown"
       ;; In theory, we can implement vars. mp:map-process-backtrace
       ;; takes one more undocumented keyword argument :frame-func
       ;; that gets a frame object. In practice it's hard to get
       ;; right, because the objects can have loops.
       :vars nil)))))


(defun encode-stacktrace (condition json-stream &optional (sentry-client *sentry-client*))
  "Encode the stacktrace as a plain string for now"
  (flet ((encode-frame (frame)
           (json:with-object (json-stream)
             (json:encode-object-member "function" (princ-to-string (trivial-backtrace::frame-func frame)) json-stream)
             (json:as-object-member ("vars" json-stream)
               (json:with-object (json-stream)
                 (loop for var in (trivial-backtrace::frame-vars frame)
                       do
                          (json:encode-object-member (princ-to-string (trivial-backtrace::var-name var))
                                                     (princ-to-string (trivial-backtrace::var-value var))
                                                     json-stream))))
             (json:encode-object-member "filename" (trivial-backtrace::frame-source-filename frame))
             ;;(json:encode-object-member "lineno" (trivial-backtrace::frame-source-pos frame))
             )))
    (let ((frames nil))
      (let ((func (lambda (frame) (push frame frames))))
        #-lispworks
        (trivial-backtrace:map-backtrace func)

        #+lispworks
        (lw-map-backtrace func))
      (json:with-object (json-stream)
        (json:as-object-member ("frames" json-stream)
          (json:with-array (json-stream)
            (loop for frame in frames do
              (json:as-array-member (json-stream)
                (encode-frame frame)))))))))

(defun capture-exception (condition &rest args)
  (apply #'client-capture-exception *sentry-client* condition args))

(defun encode-exception-event (condition &key (sentry-client *sentry-client*) extras)
  (with-output-to-string (json:*json-output*)
    (json:with-object ()
      (encode-core-event-attributes json:*json-output*
                                    :sentry-client sentry-client
                                    :extras extras)
      (json:as-object-member ("exception")
        (json:with-array ()
          (json:as-array-member ("values")
            (json:with-object ()
              (encode-exception condition json:*json-output*
                                sentry-client))))))))

(defmethod client-capture-exception ((sentry-client sentry-client) condition &key extras)
  (post-sentry-request (encode-exception-event condition
                                               :sentry-client sentry-client
                                               :extras extras)
                       sentry-client))

(defmacro with-sentry-error-handler ((&key (resignal t)) &body body)
  `(handler-case (progn ,@body)
     (error (e)
       (sentry-client:capture-exception e)
       ,@(when resignal
           `((error e))))))

(defun test-sentry-client (datum &rest args)
  (handler-case (error datum args)
    (error (e)
      (capture-exception e))))
