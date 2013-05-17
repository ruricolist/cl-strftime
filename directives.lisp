(in-package #:cl-strftime)

(declaim (optimize speed))

(deftype month () '(integer 1 12))

(deftype day () '(integer 1 31))

(deftype weekday () '(integer 1 7))

(deftype hour () '(integer 1 24))

(deftype year () 'fixnum)

(defparameter *time-directives*
  (alist-hash-table
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
     (#\S timestamp-second 2 #\0)
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
     (#\Z time-zone-name))))

(defun find-directive (char)
  (values-list (gethash char *time-directives*)))

(defparameter *named-time-formats*
  (alist-hash-table
   '((:arpa :rfc-822)
     (:asctime "%a %b %_d %T %Y")
     (:atom :rfc-3339)
     (:cookie :rfc-850 :gmt t)
     (:ctime :asctime)
     (:email :rfc-822)
     (:html :w3c)
     ;; HTTP says it uses RFC 1123, but specifies two-digit days of the
     ;; month.
     (:http "%a, %d %b %Y %T GMT" :gmt t)
     (:news :rfc-850)
     ;; Strictly speaking there is more than one RFC 822 format, but
     ;; this is the one most often used, and formalized by RFC 2822.
     (:rfc-822 "%a, %-d %b %Y %T %z")
     (:rfc-850 "%a, %-d-%b-%y %T %Z")
     (:rfc-1036 :rfc-822)
     ;; TODO 1123
     (:rfc-1123 "%a, %-d %b %Y %T %z")
     (:rfc-1132 "%a, %-d %b %Y %T %z")
     (:rfc-2086 :rfc-2822 :gmt t)
     (:rfc-2616 :http)
     (:rfc-2822 :rfc-822)
     (:rfc-3339 :w3c)
     (:rss :rfc-2822)
     (:unix "%s")
     (:usenet :news)
     (:w3c "%FT%T%z")
     (:xml :w3c))))

(defun named-time-format (name &optional gmt)
  (declare (optimize speed))
  (destructuring-bind (format &key (gmt gmt))
      (gethash name *named-time-formats*)
    (unless format
      (error "~A is not a known time format" name))
    (if (symbolp format)
        (named-time-format format gmt)
        (values format gmt))))

(defparameter *months*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun month-name (time)
  (svref *months* (1- (the month (timestamp-month time)))))

(defparameter *short-months*
  (map 'vector (lambda (month) (subseq (the string month) 0 3)) *months*))

(defun short-month-name (time)
  (svref *short-months* (1- (the month (timestamp-month time)))))

(defparameter *weekdays*
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(defun weekday-name (time)
  (svref *weekdays* (timestamp-day-of-week time)))

(defparameter *short-weekdays*
  (map 'vector (lambda (day) (subseq (the string day) 0 3)) *weekdays*))

(defun short-weekday-name (time)
  (svref *short-weekdays* (timestamp-day-of-week time)))

(defun o-clock (time)
  (mod (the hour (timestamp-hour time)) 12))

(defun year-of-century (time)
  (rem (the year (timestamp-year time)) 100))

(defun day-of-year (time)
  (date-calc:day-of-year
   (timestamp-year time)
   (timestamp-month time)
   (timestamp-day time)))

(defun am/pm (time)
  (if (>= (the hour (timestamp-hour time)) 12)
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
  (rem (the year (iso-8601-year time)) 100))

(defun iso-8601-week-and-year (time)
  (let ((year (timestamp-year time))
        (month (timestamp-month time))
        (day (timestamp-day time)))
    (declare (year year) (month month) (day day))
    (multiple-value-bind (y1 m1 d1)
        (date-calc:add-delta-days year 1 4
                                  (- (the weekday (date-calc:cl-day-of-week year 1 4))))
      (values
       (1+ (floor
            (date-calc:delta-days y1 m1 d1 year month day)
            7))
       y1))))

(defun time-zone-offset (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let ((offset (timestamp-subtimezone time *default-timezone*)))
    (format nil "~:[-~;+~]~4,'0D"
            (plusp offset)
            (abs (* 100 (floor offset #.(* 60 60)))))))

(defun time-zone-offset-with-colon (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let ((offset (timestamp-subtimezone time *default-timezone*)))
    (multiple-value-bind (hours minutes)
        (floor offset #.(* 60 60))
      (format nil "~:[-~;+~]~2,'0D:~2,'0D"
              (plusp offset)
              (abs hours) (abs minutes)))))

(defun time-zone-name (time &optional nato)
  (if (and nato (eq *default-timezone* +utc-zone+))
      "Z"
      (let ((name
              (nth-value 10 (decode-timestamp time))))
        (if (equal name "UTC")
            "GMT"
            name))))
