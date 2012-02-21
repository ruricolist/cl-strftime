(in-package #:cl-strftime)

(defun flatten! (list)
  (mapcan
   (lambda (elt)
     (if (atom elt)
         (list elt)
         (flatten! elt)))
   list))

(defun toggle-case (string)
  (map-into (make-string (length string))
            (lambda (c)
              (if (both-case-p c)
                  (cond ((upper-case-p c)
                         (char-downcase c))
                        ((lower-case-p c)
                         (char-upcase c)))
                  c))
            string))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defstruct (universal-time (:type list)
                           (:conc-name nil))
  seconds minutes hours day-of-month month year day-of-week daylight? timezone)

(defparameter *directive-regex*
  (ppcre:create-scanner
   "%([-_0^#]?)([0-9]*)([a-zA-Z]|%)"
   :single-line-mode t
   :case-insensitive-mode nil))

(defparameter *months*
  '("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defparameter *short-months*
  (mapcar (lambda (m) (subseq m 0 3)) *months*))

(defparameter *weekdays*
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defparameter *short-weekdays*
  (mapcar (lambda (m) (subseq m 0 3)) *weekdays*))

(defun epoch-time (time)
  (- (reencode-universal-time time) +unix-epoch+))

(defun mod-24-hours (time)
  (hours time))

(defun mod-12-hours (time)
  (mod (hours time) 12))

(defun weekday-name (time)
  (nth (day-of-week time) *weekdays*))

(defun short-weekday-name (time)
  (nth (day-of-week time) *short-weekdays*))

(defun month-name (time)
  (nth (month time) *months*))

(defun short-month-name (time)
  (nth (month time) *short-months*))

(defun century (time)
  (floor (year time) 100))

(defun day-of-year (time)
  (date-calc:day-of-year
   (year time)
   (month time)
   (day-of-month time)))

(defun year-of-century (time)
  (rem (year time) 100))

(defun am/pm (time)
  (if (>= (hours time) 12)
      "PM"
      "AM"))

(defun week-number (time)
  (date-calc:week-of-year
   (year time)
   (month time)
   (day-of-month time)))

(defun business-day-of-week (time)
  (date-calc:day-of-week
   (year time)
   (month time)
   (day-of-month time)))

(defun business-week-number (time)
  (date-calc:week-of-year
   (year time)
   (month time)
   (day-of-month time)))

(defun parse-universal-time (time)
  (multiple-value-list (decode-universal-time time)))

(defun reencode-universal-time (time)
  (encode-universal-time
   (seconds time)
   (minutes time)
   (hours time)
   (day-of-month time)
   (month time)
   (year time)
   (timezone time)))

(defun parse-iso-8601 (time)
  (multiple-value-bind (y1 m1 d1)
      (date-calc:add-delta-days (year time) 1 4
                                (- (date-calc:cl-day-of-week (year time) 1 4)))
    (values
     (1+ (floor
          (date-calc:delta-days y1 m1 d1
                                (year time) (month time) (day-of-month time))
          7))
     y1)))

(defun iso-8601-week-number (time)
  (values (parse-iso-8601 time)))

(defun iso-8601-year (time)
  ;; Using nth-value confuses SBCL here.
  (multiple-value-bind (week-number year)
      (parse-iso-8601 time)
    (declare (ignore week-number))
    year))

(defun iso-8601-year-of-century (time)
  (rem (iso-8601-year time) 100))

(defun time-zone-offset (time)
  (let ((tz (timezone time)))
    (format nil
            "~:[+~;-~]~4,'0D"
            ;; CL gives time zones the opposite sign as RFC 2822.
            (plusp tz)
            (floor (* tz 100)))))

(defparameter *directives*
  ;; Characters are inserted literally; strings are recursed on as
  ;; time formats; symbols are called as functions with the parsed
  ;; time. Lists are interpreted as calls to ~D; the value of the
  ;; third element, a function, is padded to the width given by the
  ;; second element with the character given by the first.
  '((#\% . #\%)
    (#\A . weekday-name)
    (#\a . short-weekday-name)
    (#\B . month-name)
    (#\b . short-month-name)
    (#\C . century)
    (#\d . (#\0 2 day-of-month))
    (#\D . "%m/%d/%y")
    (#\e . "%_2d")
    (#\F . "%Y-%m-%d")
    (#\G . iso-8601-year)
    (#\g . (#\0 2 iso-8601-year-of-century))
    (#\h . "%b")
    (#\H . (#\0 2 mod-24-hours))
    (#\I . (#\0 2 mod-12-hours))
    (#\j . (#\0 3 day-of-year))
    (#\k . (#\_ 2 mod-24-hours))
    (#\l . (#\_ 2 mod-12-hours))
    (#\m . (#\0 2 month))
    (#\M . (#\0 2 minutes))
    (#\n . #\Newline)
    (#\p . am/pm)
    (#\P . "%#p")
    (#\r . "%I:%M:%S %p")
    (#\R . "%H:%M")
    (#\s . epoch-time)
    (#\S . (#\0 2 seconds))
    (#\t . #\Tab)
    (#\T . "%H:%M:%S")
    (#\u . business-day-of-week)
    (#\U . (#\0 2 week-number))
    (#\V . (#\0 2 iso-8601-week-number))
    (#\w . day-of-week)
    (#\W . (#\0 2 business-week-number))
    (#\y . year-of-century)
    (#\Y . year)
    (#\z . time-zone-offset)
    (#\Z . timezone)))

(defmacro make-writer (&body body)
  `(function
    (lambda (stream time)
     (declare (ignorable time))
     ,@body)))

(defun compile-time-format (string)
  (let (writers)
    (labels ((parse (&key (start 0))
               (multiple-value-bind (ms me rs re)
                   (ppcre:scan *directive-regex* string :start start)
                 (if ms
                     (progn
                       (push (make-writer
                               (write-string string stream :start start :end ms))
                             writers)
                       (push (apply #'compile-directive
                                    (loop for s across rs
                                          for e across re
                                          collect (unless (= s e)
                                                    (subseq string s e))))
                             writers)
                       (parse :start me))
                     (push (make-writer
                             (write-string string stream :start start))
                           writers)))))
      (parse))
    writers))

(defun make-time-formatter (string)
  (let ((writers (nreverse (flatten! (compile-time-format string)))))
    (lambda (stream &key (time (get-universal-time)))
      (let ((time (parse-universal-time time)))
        (dolist (fn writers)
          (funcall fn stream time))))))

(defun compile-directive (flag width directive)
  (let* ((action (cdr (assoc (character directive)
                             *directives*
                             :test #'char=)))
         (flag-char (if flag
                        (character flag)
                        (when (consp action)
                          (first action))))
         (width-size (if width
                         (parse-integer width)
                         (when (consp action)
                           (second action)))))
    (cond ((null action)
           (error "~S is not a known directive" directive))
          ((characterp action)
           (make-writer (write-char action stream)))
          ((stringp action)
           (compile-time-format action))
          (t (let ((fn (if (consp action) (third action) action)))
               (case flag-char
                 (#\- (make-writer (format stream "~A"
                                           (funcall fn time))))
                 (#\_ (make-writer (format stream "~V,VD"
                                           width-size #\Space
                                           (funcall fn time))))
                 (#\0 (make-writer (format stream "~V,VD"
                                           width-size #\0
                                           (funcall fn time))))
                 (#\^ (make-writer (format stream "~:@(~A~)"
                                           (funcall fn time))))
                 (#\# (make-writer
                       (let ((value (funcall fn time)))
                         (format stream "~A"
                                 (if (stringp value)
                                     (toggle-case value)
                                     value)))))
                 (t
                  (make-writer
                   (format stream "~A"
                           (funcall fn time))))))))))

(defun format-time (stream format &optional (time (get-universal-time)))
  "Write TIME to STREAM as instructed by FORMAT."
  (let ((formatter
          (if (functionp format)
              format
              (make-time-formatter format))))
    (if stream
        (funcall formatter stream :time time)
        (with-output-to-string (s)
          (funcall formatter s :time time)))))

(define-compiler-macro format-time (&whole decline stream format &optional time
                                           &environment env)
  (if (constantp format env)
      `(format-time ,stream (load-time-value (make-time-formatter ,format))
                    ,@(when time (list time)))
      decline))
