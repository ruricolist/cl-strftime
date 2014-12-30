(in-package #:cl-srtftime)

(defcondition strftime-condition (condition) ())

(defcondition strftime-error (error strftime-condition) ())

(defcondition unknown-directive ()
  ((char :initarg :char :type character
         :reader unknown-directive-char
         :reader char-of))
  (:documentation "An unknown directive character.")
  (:report (lambda (c s)
             (format s "Unknown directive: ~c" (char-of c)))))

(defcondition unknown-format ()
  ((name :initarg :name :type :keyword
         :reader unknown-format-name
         :reader name-of))
  (:documentation "An unknown named format.")
  (:report (lambda (c s)
             (format s "Unknown format: ~a" (name-of c)))))
