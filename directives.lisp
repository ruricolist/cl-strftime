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
    (#\Z . time-zone-name)))

(defparameter *named-time-formats*
  '((arpa rfc-822)
    (asctime "%a %b %_d %H %Y")
    (atom rfc-3339)
    (cookie rfc-850 :gmt t)
    (ctime asctime)
    (email rfc-822)
    (html w3c)
    ;; HTTP says it uses RFC 1123, but specifies two-digit days of the
    ;; month.
    (http "%a, %d %b %Y %T GMT" :gmt t)
    (news rfc-850)
    ;; Strictly speaking there is more than one RFC 822 format, but
    ;; this is the one most often used, and formalized by RFC 2822.
    (rfc-822 "%a, %-d %b %Y %T %z")
    (rfc-850 "%a, %-d-%b-%y %T %Z")
    (rfc-1036 rfc-2822)
    (rfc-1132 "%a, %-d %b %Y %T %z")
    (rfc-2086 rfc-2822 :gmt t)
    (rfc-2616 http)
    (rfc-2822 rfc-822)
    (rfc-3339 w3c)
    (rss rfc-2822)
    (unix "%s")
    (usenet news)
    (w3c "%FT%T%z")
    (xml w3c)))

(defun named-time-format (name &optional gmt)
  (destructuring-bind (name format &key (gmt gmt))
      (assoc (string name) *named-time-formats* :key #'string :test #'equal)
    (declare (ignore name))
    (if (symbolp format)
        (named-time-format format gmt)
        (values format gmt))))

(defparameter *us-time-zones*
  '((-5 "EST" "EDT")
    (-6 "CST" "CDT")
    (-7 "MST" "MDT")
    (-8 "PSD" "PDT"))
  "Time zones known to RFC 822.")

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun epoch-time (time)
  (- (reencode-universal-time time) +unix-epoch+))

(defstruct (universal-time (:type list)
                           (:conc-name nil)
                           (:constructor nil))
  seconds minutes hours day-of-month month year day-of-week daylight? timezone)

(defun make-universal-time (&optional time tz)
  (let ((time (or time (get-universal-time))))
    (if tz
        (make-tz time tz)
        (multiple-value-list (decode-universal-time time)))))

(defun make-tz (utime tz)
  (multiple-value-bind (s min h dom mon y dow daylight? zone)
      (decode-universal-time utime (if (numberp tz) tz 0))
    (declare (ignore daylight? zone))
    (list s min h dom mon y dow nil tz)))

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
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun month-name (time)
  (svref *months* (1- (month time))))

(defun short-month-name (time)
  (subseq (month-name time) 0 3))

(defparameter *weekdays*
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun weekday-name (time)
  (svref *weekdays* (day-of-week time)))

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
  ;; CL gives time zones the opposite sign as ISO.
  (let ((tz (- (timezone time))))
    (when (daylight? time)
      (incf tz))
    (format nil "~:[-~;+~]~4,'0D"
            (plusp tz)
            (abs (* tz 100)))))

(defun time-zone-offset-with-colon (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let ((tz (- (timezone time))))
    (when (daylight? time)
      (incf tz))
    (multiple-value-bind (quot rem)
        (floor (abs tz))
      (format nil "~:[-~;+~]~2,'0D:~2,'0D"
              (plusp tz)
              quot rem))))

(defun time-zone-name (time &optional nato)
  (let ((tz (timezone time)))
    (cond ((zerop tz)
           (if nato "Z" "GMT"))
          ((find (- tz) *us-time-zones* :key #'car :test #'=)
           (funcall
            (if (daylight? time)
                #'third
                #'second)
            (assoc (- tz) *us-time-zones* :test #'=)))
          (t (princ-to-string tz)))))
