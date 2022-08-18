(defpackage :sentry-client.hunchentoot
  (:use :cl :sentry-client)
  (:export :*request-extras*
   :*session-extras*))

(in-package :sentry-client.hunchentoot)

(defvar *request-extras*
  '((:request-uri . hunchentoot:request-uri)
    (:request-method . hunchentoot:request-method)
    (:request-headers . hunchentoot:headers-in))
  "An alist of keys and request accessor functions for extracting information from current request and adding to Stripe exception.")

(defvar *session-extras* nil
  "If T, then all session values are exposed.
If a list of keys, then only the values of those keys in current session are exposed.
If nil, then no session information is exposed.")

(defun request-extras ()
  "Collect extras from Hunchentoot request."
  (let ((req hunchentoot:*request*))
    (loop for (key . accessor) in *request-extras*
          collect (cons key (princ-to-string (funcall accessor req))))))

(defun session-extras ()
  "Collect extras from Hunchentoot session."
  (cond
    ((eq *session-extras* t)
     (loop for (key . value) in hunchentoot:*session*
	   collect (cons key (princ-to-string value))))
    ((listp *session-extras*)
     (loop for key in *session-extras*
           collect (cons key (princ-to-string (hunchentoot:session-value key)))))))

(defmethod hunchentoot:maybe-invoke-debugger :after (condition)
  (when (and hunchentoot:*catch-errors-p* sentry-client:*sentry-client*)
    #-sbcl(sentry-client:capture-exception condition)
    #+sbcl
    (let ((sb-debug:*stack-top-hint* nil))
      (sentry-client:capture-exception
       condition
       :transaction (format nil "~a ~a"
                            (hunchentoot:request-method*)
                            (hunchentoot:request-uri*))
       :extras (append (request-extras)
                       (session-extras))))))
