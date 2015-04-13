;;;; package.lisp

(defpackage #:cl-strftime
  (:use #:cl #:alexandria #:serapeum #:local-time)
  (:shadow #:time #:second)
  (:export #:make-time-formatter #:format-time
           #:strftime-error
           #:unknown-directive #:unknown-format
           #:unknown-directive-char #:unknown-format-name))
