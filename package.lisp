;;;; package.lisp

(defpackage #:cl-strftime
  (:use #:cl #:local-time)
  (:export :make-time-formatter :format-time))
