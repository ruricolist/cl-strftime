;;;; package.lisp

(defpackage #:cl-strftime
  (:use #:cl #:alexandria #:local-time)
  (:export :make-time-formatter :format-time))
