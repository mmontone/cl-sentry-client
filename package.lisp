(defpackage #:sentry-client
  (:use #:cl)
  (:export #:initialize-sentry-client
           #:capture-exception
           #:capture-message
           #:with-sentry-error-handler))
