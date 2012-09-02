(in-package #:cl-strftime)

(defparameter *time-directives*
  ;; Characters are inserted literally; strings are recursed on;
  ;; symbols are called as functions with the parsed time. Lists are
  ;; interpreted as arguments to ~V,VD: (mincol padchar arg).
  '((#\% #\%)
    (#\A weekday-name)
    (#\a short-weekday-name)
    (#\B month-name)
    (#\b short-month-name)
    (#\C timestamp-century)
    (#\d timestamp-day 2 #\0)
    (#\D "%m/%d/%y")
    (#\e "%_2d")
    (#\F "%Y-%m-%d")
    (#\G iso-8601-year)
    (#\g iso-8601-year-of-century 2 #\0)
    (#\h "%b")
    (#\H timestamp-hour 2 #\0)
    (#\I o-clock 2 #\0)
    (#\j day-of-year 3 #\0)
    (#\k timestamp-hour 2 #\_)
    (#\l o-clock 2 #\_)
    (#\m timestamp-month 2 #\0)
    (#\M timestamp-minute 2 #\0)
    (#\n #\Newline)
    (#\p am/pm)
    (#\P "%#p")
    (#\r "%I:%M:%S %p")
    (#\R "%H:%M")
    (#\s timestamp-to-unix)
    (#\S timestamp-second #\0 2)
    (#\t #\Tab)
    (#\T "%H:%M:%S")
    (#\u business-day-of-week)
    (#\U week-number 2 #\0)
    (#\V iso-8601-week-number 2 #\0)
    (#\w timestamp-day-of-week)
    (#\W business-week-number 2 #\0)
    (#\y year-of-century)
    (#\Y timestamp-year)
    (#\z time-zone-offset)
    (#\Z time-zone-name)))

(defun find-directive (char)
  (values-list (cdr (assoc char *time-directives* :test #'char=))))

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

(defparameter *months*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun month-name (time)
  (svref *months* (1- (timestamp-month time))))

(defun short-month-name (time)
  (subseq (month-name time) 0 3))

(defparameter *weekdays*
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun weekday-name (time)
  (svref *weekdays* (timestamp-day-of-week time)))

(defun short-weekday-name (time)
  (subseq (weekday-name time) 0 3))

(defun o-clock (time)
  (mod (timestamp-hour time) 12))

(defun year-of-century (time)
  (rem (timestamp-year time) 100))

(defun day-of-year (time)
  (date-calc:day-of-year
   (timestamp-year time)
   (timestamp-month time)
   (timestamp-day time)))

(defun am/pm (time)
  (if (>= (timestamp-hour time) 12)
      "PM"
      "AM"))

(defun week-number (time)
  (date-calc:week-of-year
   (timestamp-year time)
   (timestamp-month time)
   (timestamp-day time)))

(defun business-day-of-week (time)
  (date-calc:day-of-week
   (timestamp-year time)
   (timestamp-month time)
   (timestamp-day time)))

(defun business-week-number (time)
  (date-calc:week-of-year
   (timestamp-year time)
   (timestamp-month time)
   (timestamp-day time)))

(defun iso-8601-week-number (time)
  (nth-value 0 (iso-8601-week-and-year time)))

(defun iso-8601-year (time)
  (nth-value 1 (iso-8601-week-and-year time)))

(defun iso-8601-year-of-century (time)
  (rem (iso-8601-year time) 100))

(defun iso-8601-week-and-year (time)
  (multiple-value-bind (y1 m1 d1)
      (date-calc:add-delta-days (year time) 1 4
                                (- (date-calc:cl-day-of-week (year time) 1 4)))
    (values
     (1+ (floor
          (date-calc:delta-days y1 m1 d1
                                (year time) (month time) (day-of-month time))
          7))
     y1)))

(defun time-zone-offset (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let ((offset (timestamp-subtimezone time *default-timezone*)))
    (format nil "~:[-~;+~]~4,'0D"
            (plusp offset)
            (abs (* 100 (floor offset #.(* 60 60)))))))

(defun time-zone-offset-with-colon (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let* ((offset (timestamp-subtimezone time *default-timezone*)))
    (multiple-value-bind (hours minutes)
        (floor offset #.(* 60 60))
      (format nil "~:[-~;+~]~2,'0D:~2,'0D"
              (plusp offset)
              (abs hours) (abs minutes)))))

(defun time-zone-name (time &optional nato)
  (if (and nato (eql *default-timezone* +utc-zone+))
      "Z"
      (nth-value 10 (decode-timestamp time))))
