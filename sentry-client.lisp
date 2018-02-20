(in-package #:sentry-client)

(defvar *sentry-client*)

(defparameter +dsn-regex+ "(.*)\\:\\/\\/(.*)\\:(.*)@(.*)\\/(.*)")

(defun parse-dsn (dsn-string)
  "'{PROTOCOL}://{PUBLIC_KEY}:{SECRET_KEY}@{HOST}/{PATH}{PROJECT_ID}'"
  (ppcre:register-groups-bind (protocol public-key secret-key host project-id)
      (+dsn-regex+ dsn-string)
    (when (not (every (alexandria:compose 'not 'null)
                      (list protocol public-key
                            secret-key host project-id)))
      (error "Bad DSN format"))
    (return-from parse-dsn
      (list :protocol protocol
            :public-key public-key
            :secret-key secret-key
            :host host
            :project-id project-id
            :uri (concatenate 'string protocol "://" host))))
  (error "Bad DSN format"))

(defun read-dsn (dsn)
  (cond
    ((stringp dsn) (parse-dsn dsn))
    ((consp dsn) dsn)
    (t (error "Invalid DSN: ~A" dsn))))

(defclass sentry-client ()
  ((dsn :initarg :dsn
        :initform (error "Provide the DSN")
        :accessor dsn)
   (tags :initarg :tags
         :initform nil
         :accessor sentry-tags)
   (transport :initarg :transport
              :initform :http
              :accessor sentry-transport)
   (connection-timeout :initarg :connection-timeout
                       :initform 10
                       :accessor connection-timeout)))

(defun make-sentry-client (dsn &key tags)
  (make-instance 'sentry-client
                 :dsn (read-dsn dsn)
                 :tags tags))

(defun initialize-sentry-client (dsn &rest args &key tags)
  (setf *sentry-client* (apply #'make-sentry-client dsn args)))

(defun call-with-sentry-client (function dsn &rest args &key tags)
  (let ((*sentry-client* (apply #'make-sentry-client dsn args)))
    (funcall function)))

(defmacro with-sentry-client ((dsn &rest args &key tags) &body body)
  `(call-with-sentry-client (lambda () ,@body) ,dsn ,@args))

(defun test-sentry-client (&optional (sentry-client *sentry-client*)))

(defun sentry-api-url (&optional (sentry-client *sentry-client*))
  (concatenate 'string (getf (dsn sentry-client) :uri) "/api/"
               (getf (dsn sentry-client) :project-id) "/store/")) 

(defparameter +sentry-timestamp-format+ 
  (append local-time:+iso-8601-date-format+
          (list #\T) local-time:+iso-8601-time-format+))

(defun format-sentry-timestamp (stream timestamp)
  (local-time:format-timestring stream
                                (local-time:now)
                                :format +sentry-timestamp-format+
                                :timezone local-time:+UTC-ZONE+))

(defun post-sentry-request (data &optional (sentry-client *sentry-client*))
  (drakma:http-request (sentry-api-url)
                       :method :post
                       :content-type "application/json"
                       :content data
                       :additional-headers
                       (list
                        (cons "X-Sentry-Auth"
                              (fmt:fmt nil "Sentry sentry_version=" 5 ","
                                       "sentry_client=" "cl-sentry-client/"
                                       (asdf:component-version (asdf:find-system :sentry-client)) ","
                                       "sentry_timestamp=" (format-sentry-timestamp nil (local-time:now)) ","
                                       "sentry_key=" (getf (dsn sentry-client) :public-key) ","
                                       "sentry_secret=" (getf (dsn sentry-client) :secret-key))))
                       :connection-timeout (connection-timeout sentry-client)))

(defun make-sentry-event-id ()
  (ironclad:byte-array-to-hex-string
   (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))

(defun encode-core-attributes (json-stream &optional (sentry-client *sentry-client*))
  (json:encode-object-member "event_id" (make-sentry-event-id) json-stream)
  (json:encode-object-member "timestamp" (format-sentry-timestamp nil (local-time:now)) json-stream)
  (json:encode-object-member "logger" "cl-sentry-client" json-stream)
  (json:encode-object-member "platform" "other" json-stream)
  (json:as-object-member ("sdk" json-stream)
    (json:with-object (json-stream)
      (json:encode-object-member "name" "cl-sentry-client")
      (json:encode-object-member "version" (asdf:component-version (asdf:find-system :sentry-client))))))

(defun encode-exception (condition json-stream &optional (sentry-client *sentry-client*))
  (json:encode-object-member "type" (type-of condition) json-stream)
  (json:encode-object-member "value" (princ-to-string condition) json-stream)
  (json:encode-object-member "module" (princ-to-string (symbol-package (type-of condition))) json-stream)
  (json:as-object-member ("stacktrace") 
    (encode-stacktrace condition json-stream sentry-client)))

(defun encode-stacktrace (condition json-stream &optional (sentry-client *sentry-client*))
  (json:encode-json "" json-stream))

(defun capture-exception (condition &key tags)
  (let ((json (with-output-to-string (json:*json-output*)
                (json:with-object ()
                  (encode-core-attributes json:*json-output*
                                          *sentry-client*)
                  (json:as-object-member ("exception")
                    (json:with-object ()
                      (encode-exception condition json:*json-output*
                                        *sentry-client*)))))))
    (post-sentry-request json *sentry-client*)))

(defun capture-message (message &key tags)
  )

(defmacro with-sentry-error-handler (options &body body)
  `(handler-case (progn ,@body)
     (error (e)
       (sentry-client:capture-exception e))))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))
