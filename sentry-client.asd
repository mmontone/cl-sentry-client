(asdf:defsystem #:sentry-client
  :description "Sentry client"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :homepage "https://github.com/mmontone/cl-sentry-client"
  :long-description #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))              
  :serial t
  :depends-on (:dexador
               :cl-json
               :cl-ppcre
               :uuid
               :local-time
               :trivial-backtrace
               :babel
               :salza2)
  :components ((:file "package")
               (:file "sentry-client")))
