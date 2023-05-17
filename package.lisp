(defpackage #:sentry-client
  (:use #:cl)
  (:export #:*request*
           #:make-sentry-request
           #:make-sentry-request-headers
           #:initialize-sentry-client
           #:test-sentry-client
           #:capture-exception
           #:capture-message
           #:with-sentry-error-handler
           #:sentry-client
           #:async-sentry-client
	   #:*sentry-client*
	   #:sentry-tags)
  (:documentation #.(uiop:read-file-string
		     (uiop:subpathname (or *compile-file-pathname*
					   *load-pathname*)
				       "README.md"))))
