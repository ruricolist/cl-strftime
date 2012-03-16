(in-package #:cl-strftime)

(defparameter *time-directives*
  ;; Characters are inserted literally; strings are recursed on;
  ;; symbols are called as functions with the parsed time. Lists are
  ;; interpreted as arguments to ~V,VD: (mincol padchar arg).
  '((#\% . #\%)
    (#\A . weekday-name)
    (#\a . short-weekday-name)
    (#\B . month-name)
    (#\b . short-month-name)
    (#\C . century)
    (#\d . (2 #\0 day-of-month))
    (#\D . "%m/%d/%y")
    (#\e . "%_2d")
    (#\F . "%Y-%m-%d")
    (#\G . iso-8601-year)
    (#\g . (2 #\0 iso-8601-year-of-century))
    (#\h . "%b")
    (#\H . (2 #\0 mod-24-hours))
    (#\I . (2 #\0 mod-12-hours))
    (#\j . (3 #\0 day-of-year))
    (#\k . (2 #\_ mod-24-hours))
    (#\l . (2 #\_ mod-12-hours))
    (#\m . (2 #\0 month))
    (#\M . (2 #\0 minutes))
    (#\n . #\Newline)
    (#\p . am/pm)
    (#\P . "%#p")
    (#\r . "%I:%M:%S %p")
    (#\R . "%H:%M")
    (#\s . epoch-time)
    (#\S . (2 #\0 seconds))
    (#\t . #\Tab)
    (#\T . "%H:%M:%S")
    (#\u . business-day-of-week)
    (#\U . (2 #\0 week-number))
    (#\V . (2 #\0 iso-8601-week-number))
    (#\w . day-of-week)
    (#\W . (2 #\0 business-week-number))
    (#\y . year-of-century)
    (#\Y . year)
    (#\z . time-zone-offset)
    (#\Z . timezone)))

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun epoch-time (time)
  (- (reencode-universal-time time) +unix-epoch+))

(defstruct (universal-time (:type list)
                           (:conc-name nil)
                           (:constructor nil))
  seconds minutes hours day-of-month month year day-of-week daylight? timezone)

(defun make-universal-time (time)
  (multiple-value-list (decode-universal-time time)))

(defun reencode-universal-time (time)
  (encode-universal-time
   (seconds time)
   (minutes time)
   (hours time)
   (day-of-month time)
   (month time)
   (year time)
   (- (timezone time)
      (if (daylight? time)
          1 0))))

(defparameter *months*
  '("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun month-name (time)
  (nth (1- (month time)) *months*))

(defun short-month-name (time)
  (subseq (month-name time) 0 3))

(defparameter *weekdays*
  '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun weekday-name (time)
  (nth (day-of-week time) *weekdays*))

(defun short-weekday-name (time)
  (subseq (weekday-name time) 0 3))

(defun mod-24-hours (time)
  (hours time))

(defun mod-12-hours (time)
  (mod (hours time) 12))

(defun century (time)
  (floor (year time) 100))

(defun year-of-century (time)
  (rem (year time) 100))

(defun day-of-year (time)
  (date-calc:day-of-year
   (year time)
   (month time)
   (day-of-month time)))

(defun am/pm (time)
  (if (>= (mod-24-hours time) 12)
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

(defun iso-8601-week-number (time)
  (car (iso-8601-week-and-year time)))

(defun iso-8601-year (time)
  (cdr (iso-8601-week-and-year time)))

(defun iso-8601-year-of-century (time)
  (rem (iso-8601-year time) 100))

(defun iso-8601-week-and-year (time)
  (multiple-value-bind (y1 m1 d1)
      (date-calc:add-delta-days (year time) 1 4
                                (- (date-calc:cl-day-of-week (year time) 1 4)))
    (cons
     (1+ (floor
          (date-calc:delta-days y1 m1 d1
                                (year time) (month time) (day-of-month time))
          7))
     y1)))

(defun time-zone-offset (time)
  ;; CL gives time zones the opposite sign as RFC 2822.
  (let ((tz (- (timezone time))))
    (when (daylight? time)
      (incf tz))
    (format nil "~@D" (floor (* tz 1000)))))
