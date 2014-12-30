;;;; cl-strftime.asd

(asdf:defsystem #:cl-strftime
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "Common Lisp compiler for the strftime language."
  :license "MIT"
  :serial t
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-strftime/tests)))
  :depends-on (#:alexandria
               #:serapeum
               #:cl-ppcre
               #:local-time)
  :components ((:file "package")
               (:file "time")
               (:file "directives")
               (:file "named-format")
               (:file "math")
               (:file "cl-strftime")))

(asdf:defsystem #:cl-strftime/tests
  :pathname "tests/"
  :perform (asdf:test-op (o c) (uiop:symbol-call :cl-strftime :run-tests))
  :serial t
  :depends-on (#:cl-strftime
               #:cffi
               #:fiveam
               #:uiop)
  :components ((:file "tests")))
