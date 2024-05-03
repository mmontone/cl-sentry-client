(in-package #:sentry-client)

(defvar *sentry-client* nil
  "The current sentry client.")

(defvar *request* nil)

(defparameter +sentry-timestamp-format+
  (append local-time:+iso-8601-date-format+
          (list #\T) local-time:+iso-8601-time-format+))

(defparameter +dsn-regex+ "(.*)\\:\\/\\/(.*)\\@(.*)\\/(.*)")

(defstruct sentry-request-headers
  (accept nil :type string)
  (host nil :type string)
  (user-agent nil :type string))

(defstruct sentry-request
  "The Request interface contains information on a HTTP request related to the event.

See: https://develop.sentry.dev/sdk/event-payloads/request/"
  (url nil :type (or null string))
  (method nil :type (or null string))
  (headers nil :type (or null sentry-request-headers)))

(defstruct geo-location
  "Approximate geographical location of the end user or device.

See: https://develop.sentry.dev/sdk/event-payloads/user/"
  (city nil :type string)
  (country-code nil :type string)
  (region nil :type string))

(defstruct sentry-user
  "An interface describing the authenticated User for a request.

You should provide at least one of id, email, ip_address, username for Sentry to be able to tell you how many users are affected by one issue, for example. Sending a user that has none of these attributes and only custom attributes is valid, but not as useful.

See: https://develop.sentry.dev/sdk/event-payloads/user/"
  (id nil :type (or null string))
  (username nil :type (or null string))
  (email nil :type (or null string))
  (ip-address nil :type (or null string))
  (geo nil :type (or null geo-location))
  (extras nil :type list))

(defgeneric sentry-tags (error)
  (:documentation "Returns an alist of tags for ERROR.
User can specialize this generic function for custom CONDITION classes."))

(defmethod sentry-tags ((condition condition))
  nil)

(defgeneric condition-severity-level (condition)
  (:documentation "The condition severity level (warning, error, etc)"))

(defmethod condition-severity-level ((condition condition))
  :info)

(defmethod condition-severity-level ((condition warning))
  :warning)

(defmethod condition-severity-level ((condition error))
  :error)

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
                       :accessor connection-timeout)
   (release :initarg :release
            :initform nil
            :accessor project-release
            :documentation "The release version of the application.")
   (server-name :initarg :server-name
                :initform nil
                :accessor server-name
                :documentation "Identifies the host from which the event was recorded.")
   (environment :initarg :environment
                :initform "production"
                :accessor running-environment)
   (gzip-compression :initarg :gzip-compression
                     :initform t
                     :accessor gzip-compression-p))
  (:documentation "A sentry client"))

(defmethod initialize-instance :after ((sentry-client sentry-client) &rest initargs)
  (declare (ignore initargs))
  (setf (dsn sentry-client) (read-dsn (dsn sentry-client)))
  (setf (running-environment sentry-client)
        (or (running-environment sentry-client) "production")))

(defun make-sentry-client (dsn &optional (class 'sentry-client))
  (make-instance class :dsn (read-dsn dsn)))

(defun initialize-sentry-client (dsn &rest args &key (client-class 'sentry-client) &allow-other-keys)
  (setf *sentry-client*
        (apply #'make-instance client-class
               :dsn (read-dsn dsn)
               (alexandria:remove-from-plist args :client-class))))

(defun call-with-sentry-client (function client-class &rest args)
  "Call FUNCTION in the context of a SENTRY-CLIENT instantied from CLIENT-CLASS and ARGS."
  (let ((*sentry-client* (apply #'make-instance client-class args)))
    (funcall function)))

(defmacro with-sentry-client ((class &rest args) &body body)
  "Run BODY in the scope of a sentry client created from CLASS and ARGS."
  `(call-with-sentry-client (lambda () ,@body) ',class ,@args))

(defun sentry-api-url (&optional (sentry-client *sentry-client*))
  "The events url.

See: https://develop.sentry.dev/sdk/store/"

  (concatenate 'string (getf (dsn sentry-client) :uri) "/api/"
               (getf (dsn sentry-client) :project-id) "/store/"))

(defun format-sentry-timestamp (stream &optional timestamp)
  "Format TIMESTAMP for Sentry.
If no TIMESTAMP is provided, then current time is used."
  (local-time:format-timestring stream
                                (or timestamp (local-time:now))
                                :format +sentry-timestamp-format+
                                :timezone local-time:+UTC-ZONE+))

(defun encode-sentry-auth-header (sentry-client)
  "Encode Sentry authentication header."
  (format nil "Sentry ~:{~(~A~)=~A~:^,~}"
          `((sentry_version 5)
            (sentry_client ,(concatenate 'string "cl-sentry-client/"
                                         (asdf:component-version (asdf:find-system :sentry-client))))
            (sentry_timestamp ,(format-sentry-timestamp nil (local-time:now)))
            (sentry_key ,(getf (dsn sentry-client) :public-key)))))

(defun post-sentry-request (data &optional (sentry-client *sentry-client*))
  "Just send DATA to sentry api via HTTP."
  (if (gzip-compression-p sentry-client)
      (let ((compressed (salza2:compress-data (babel:string-to-octets data)
                                              'salza2:gzip-compressor)))
        (dex:post (sentry-api-url)
                  :content compressed
                  :headers `(("Content-Type" . "json")
                             ("Content-Encoding" . "gzip")
                             ("X-Sentry-Auth" . ,(encode-sentry-auth-header sentry-client)))
                  :connect-timeout (connection-timeout sentry-client)
                  :keep-alive nil))
      (dex:post (sentry-api-url)
                :headers `(("Content-Type" . "application/json")
                           ("X-Sentry-Auth" . ,(encode-sentry-auth-header sentry-client)))
                :content data
                :connect-timeout (connection-timeout sentry-client))))

(defun make-sentry-event-id ()
  "Create an ID for a new Sentry event."
  (ironclad:byte-array-to-hex-string
   (uuid:uuid-to-byte-array (uuid:make-v4-uuid))))

(declaim (ftype (function (condition stream &key (:extras list)
                                     (:sentry-client (or null sentry-client))
                                     (:transaction t)
                                     (:user (or null sentry-user))
                                     (:request (or null sentry-request)))
                          t)
                encode-core-event-attributes))

(defun encode-core-event-attributes (condition json-stream
                                     &key extras
                                       sentry-client
                                       transaction
                                       user
                                       (request *request*))
  "Encode core Sentry event attributes.

See: https://develop.sentry.dev/sdk/event-payloads/"

  (let ((json:*json-output* json-stream))
    (json:encode-object-member "event_id" (make-sentry-event-id))
    (json:encode-object-member "timestamp" (format-sentry-timestamp nil (local-time:now)))
    (json:encode-object-member "level" (condition-severity-level condition))
    (json:encode-object-member "logger" "cl-sentry-client")
    (when transaction
      (json:encode-object-member "transaction" transaction))
    (json:encode-object-member "platform" "other")
    (alexandria:when-let ((release (project-release sentry-client)))
      (json:encode-object-member "release" release))
    (alexandria:when-let ((server-name (server-name sentry-client)))
      (json:encode-object-member "server_name" server-name))
    (alexandria:when-let ((tags (sentry-tags condition)))
      (json:as-object-member ("tags")
        (json:encode-json-alist tags)))
    (json:encode-object-member "environment" (running-environment sentry-client))
    (when user
      (json:as-object-member ("user")
        (json:with-object ()
          (when (sentry-user-id user)
            (json:encode-object-member :id (sentry-user-id user)))
          (when (sentry-user-username user)
            (json:encode-object-member :username (sentry-user-username user)))
          (when (sentry-user-email user)
            (json:encode-object-member :email (sentry-user-email user)))
          (when (sentry-user-ip-address user)
            (json:encode-object-member "ip_address" (sentry-user-ip-address user)))
          (dolist (extra (sentry-user-extras user))
            (json:encode-object-member (car extra) (cdr extra))))))
    (when extras
      (json:as-object-member ("extra")
        (json:encode-json-alist extras)))
    (when request
      (json:as-object-member ("request" json-stream)
        (json:with-object ()
          (when (sentry-request-url request)
            (json:encode-object-member :url (sentry-request-url request)))
          (when (sentry-request-method request)
            (json:encode-object-member :method (sentry-request-method request)))
          (let ((headers (sentry-request-headers request)))
            (when headers
              (json:encode-object-member
               :headers
               `#(#("Accept" (sentry-request-headers-accept headers))
                  #("Host" ,(sentry-request-headers-host headers))
                  #("User-Agent" ,(sentry-request-headers-user-agent headers)))))))))
    (json:as-object-member ("sdk")
      (json:with-object (json-stream)
        (json:encode-object-member "name" "cl-sentry-client")
        (json:encode-object-member "version" (asdf:component-version (asdf:find-system :sentry-client)))))))

(defun encode-exception (condition json-stream &optional (sentry-client *sentry-client*))
  (declare (ignorable sentry-client))
  "Encode CONDITION into JSON-STREAM.

See: https://develop.sentry.dev/sdk/event-payloads/"
  (json:encode-object-member "type" (princ-to-string (type-of condition)) json-stream)
  (json:encode-object-member "value" (princ-to-string condition) json-stream)
  (json:encode-object-member "module" (princ-to-string (package-name (symbol-package (type-of condition)))) json-stream)
  (json:as-object-member ("stacktrace" json-stream)
    #+sbcl
    (handler-case (encode-sbcl-stacktrace json-stream)
      (error ()
        ;; fallback if an error occurs in encode-sbcl-stacktrace
        (encode-stacktrace condition json-stream sentry-client)))
    #-sbcl
    (encode-stacktrace condition json-stream sentry-client)))

#+lispworks
(defun trivial-backtrace::impl-map-backtrace (fn)
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
  "Encode stacktrace for CONDITION as JSON in JSON-STREAM.

See: https://develop.sentry.dev/sdk/event-payloads/stacktrace/"
  (declare (ignorable condition sentry-client))
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
        (trivial-backtrace:map-backtrace func))
      (json:with-object (json-stream)
        (json:as-object-member ("frames" json-stream)
          (json:with-array (json-stream)
            (loop for frame in frames do
              (json:as-array-member (json-stream)
                (encode-frame frame)))))))))

(defun capture-exception (condition &rest args)
  "Send CONDITION to Sentry.

Possible args:
- :EXTRAS: an association list with extra app specific information to encode in the Sentry event."
  (apply #'client-capture-exception *sentry-client* condition args))

(defun encode-exception-event (condition &key (sentry-client *sentry-client*) extras user transaction)
  "Encode CONDITION to a string in JSON format for Sentry.

See: https://develop.sentry.dev/sdk/event-payloads/exception/"
  (with-output-to-string (json:*json-output*)
    (json:with-object ()
      (encode-core-event-attributes condition
                                    json:*json-output*
                                    :sentry-client sentry-client
                                    :user user
                                    :extras extras
                                    :transaction transaction)
      (json:as-object-member ("exception")
        (json:with-array ()
          (json:as-array-member ("values")
            (json:with-object ()
              (encode-exception condition json:*json-output*
                                sentry-client))))))))

(defmethod client-capture-exception ((sentry-client sentry-client) condition &key extras user transaction)
  "Send CONDITION to Sentry.

EXTRAS: an association list with extra app specific information to encode in the Sentry event."
  (post-sentry-request
   (encode-exception-event condition
                           :sentry-client sentry-client
                           :user user
                           :extras extras
                           :transaction transaction)
   sentry-client))

(defmacro with-sentry-error-handler ((&rest args &key (resignal t) &allow-other-keys) &body body)
  "Setup an error handler that sends conditions signaled in BODY to Sentry.
If RESIGNAL is T (default), then the condition is resignaled after being captured by the Sentry handler."
  `(handler-case (progn ,@body)
     (error (e)
       (sentry-client:capture-exception e ,@(alexandria:remove-from-plist args :resignal))
       ,@(when resignal
           `((error e))))))

(defun test-sentry-client (datum &rest args)
  "Use for testing the sentry client.

Use: (test-sentry-client 'error \"my error\")"

  (handler-case (apply #'error datum args)
    (error (e)
      (capture-exception e))))
