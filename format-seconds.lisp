(in-package #:cl-strftime)

;; TODO First split into parts before and after the control flag.
;; Parts before the control flag should print conditionally; parts
;; after should always print.

(defun concat (&rest strings)
  "Copy STRINGS out into a single string."
  (with-output-to-string (out)
    (dolist (s strings)
      (write-string (coerce s 'string) out))))

(defun empty (object)
  (typecase object
    (null t)
    (sequence (= (length object) 0))))

(defmacro let*-values ((&rest bindings) &body body)
  (if bindings
      `(multiple-value-bind ,(caar bindings)
           ,(cadar bindings)
         (let*-values ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defparameter *specs*
  '((#\y "year" interval-years)
    (#\d "day" interval-days)
    (#\h "hour" interval-hours)
    (#\m "minute" interval-minutes)
    (#\s "second" interval-seconds)))

(defun spec-name (char)
  (second (assoc char *specs* :test #'char-equal)))

(defun spec-reader (char)
  (third (assoc char *specs* :test #'char-equal)))

(defstruct (interval (:constructor %make-interval
                         (years days hours minutes seconds)))
  years days hours minutes seconds)

(defun make-interval (seconds)
  (declare (fixnum seconds))
  (let ((years 0) (days 0) (hours 0) (minutes 0))
    (declare (fixnum years days hours minutes))
    (multiple-value-setq (minutes seconds)
      (floor seconds 60))
    (unless (zerop minutes)
      (multiple-value-setq (hours minutes)
        (floor minutes 60))
      (unless (zerop hours)
        (multiple-value-setq (days hours)
          (floor hours 24))
        (unless (zerop days)
          (multiple-value-bind (y d)
              (floor days 365.25)
            (setf years y
                  days (ceiling d))))))
    (%make-interval years days hours minutes seconds)))

(defun parse-seconds-format (string)
  (destructuring-bind (after &optional before)
      (reverse (ppcre:split "%z" string :limit 2))
    (let*-values (((cstring cdirectives)
                   (parse-conditional-directives before))
                  ((string directives)
                   (parse-directives after)))
      (values (concat cstring string)
              (append cdirectives directives)))))

(defun parse-conditional-directives (string)
  (parse-directives string t))

(defun parse-directives (string &optional conditional?)
  (let ((directives '()))
    (values
     (ppcre:regex-replace-all
      "%(\\.?)([0-9]*)(.)"
      string
      (lambda (match dot width-string spec-string)
        (declare (ignore match))
        (let ((width (when (not (empty width-string))
                       (parse-integer width-string)))
              (spec (character spec-string)))
          (push (spec-reader spec) directives)
          (compile-spec (not (empty dot)) width spec conditional?)))
      :simple-calls t)
     (nreverse directives))))

(defun compile-spec (zeropad width spec conditional?)
  (let ((number-part
          (if width
              (concat "~"
                      (princ-to-string width)
                      (if zeropad "" ",'0")
                      "D")
              "~D"))
        (name-part
          (if (upper-case-p spec)
              (concat " " (spec-name spec) "~:P")
              "")))
    (if conditional?
        (concat "~[~;~:*" number-part name-part "~]")
        (concat number-part name-part))))

(defun format-seconds (stream string seconds)
  (multiple-value-bind (string directives)
      (parse-seconds-format string)
    (let ((interval (make-interval seconds)))
      (apply #'format stream string
             (mapcar
              (lambda (fn) (funcall fn interval))
              directives)))))
