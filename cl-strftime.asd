;;;; cl-strftime.asd

(asdf:defsystem #:cl-strftime
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "Common Lisp compiler for the strftime language."
  :license "MIT"
  :serial t
  :depends-on (#:alexandria
               #:date-calc
               #:cl-ppcre
               #:local-time)
  :components ((:file "package")
               (:file "directives")
               (:file "cl-strftime")))
