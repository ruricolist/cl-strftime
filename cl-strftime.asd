;;;; cl-strftime.asd

(asdf:defsystem #:cl-strftime
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "Common Lisp compiler for the strftime language."
  :license "MIT"
  :serial t
  :depends-on (#:date-calc #:cl-ppcre)
  :components ((:file "package")
               (:file "cl-strftime")))
