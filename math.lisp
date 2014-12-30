(in-package #:cl-strftime)

(defun o-clock (time)
  (let ((hour (mod (timestamp-hour time) am/pm-hours)))
    (if (zerop hour)
        12
        hour)))

(defun year-of-century (time)
  (rem (timestamp-year time) century-years))

(defun century-of-year (time)
  (truncate (timestamp-year time) 100))

(defun day-of-year-number (y m d)
  (let ((n (aref month-offsets (1- m))))
    (if (and (leap-year? y) (> m 2))
        (+ n d 1)
        (+ n d))))

(defun timestamp-day-of-year (time)
  "The day of year of TIME, counting from 1."
  (day-of-year-number
   (timestamp-year time)
   (timestamp-month time)
   (timestamp-day time)))

(defun am/pm (time)
  (if (>= (timestamp-hour time) am/pm-hours)
      "PM"
      "AM"))

(defun am/pm-lowercase (time)
  (if (>= (timestamp-hour time) am/pm-hours)
      "pm"
      "am"))

(defun week-number (time day-of-week)
  (~> (timestamp-day-of-year time)
      1-
      (- day-of-week)
      (+ 7)
      (truncate 7)))

(defun monday-week-number (time)
  (let ((dow (1- (iso-day-of-week time))))
    (week-number time dow)))

(defun sunday-week-number (time)
  (let ((dow (timestamp-day-of-week time)))
    (week-number time dow)))

(defun leap-year? (y)
  (if (zerop (mod y 4))
      (if (zerop (mod y 100))
          (zerop (mod y 400))
          t)
      nil))

(defun iso-day-of-week (time)
  "Return the day of week from 1–7, where 1 is Monday."
  (let ((dow (timestamp-day-of-week time)))
    (if (zerop dow) 7 dow)))

(defun iso-week+year (time)
  (let* ((year (timestamp-year time))
         (ordinal (timestamp-day-of-year time))
         (weekday (iso-day-of-week time))
         (week (truncate (+ (- ordinal weekday) 10) 7)))
    (cond ((< week 1)
           (let ((year (1- year)))
             (values (last-week year) year)))
          ((> week (last-week year))
           (values 1 (1+ year)))
          (t (values week year)))))

(defun iso-8601-week-number (time)
  (nth-value 0 (iso-week+year time)))

(defun iso-8601-year (time)
  (nth-value 1 (iso-week+year time)))

(defun iso-8601-year-of-century (time)
  (rem (iso-8601-year time) century-years))

(defun time-zone-offset (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let ((offset (timestamp-subtimezone time *default-timezone*)))
    (format nil "~:[-~;+~]~4,'0D"
            (plusp offset)
            (abs (* 100 (floor offset hour-seconds))))))

(defun time-zone-offset-with-colon (time)
  ;; CL gives time zones the opposite sign as ISO.
  (let ((offset (timestamp-subtimezone time *default-timezone*)))
    (multiple-value-bind (hours minutes)
        (floor offset hour-seconds)
      (format nil "~:[-~;+~]~2,'0D:~2,'0D"
              (plusp offset)
              (abs hours) (abs minutes)))))

(defun extract-time-zone-abbrev (time)
  (nth-value 2 (timestamp-subtimezone time *default-timezone*)))

(defun time-zone-name (time &optional nato)
  (if (and nato (eq *default-timezone* +utc-zone+))
      "Z"
      (let ((name (extract-time-zone-abbrev time)))
        (if (equal name "UTC")
            "GMT"
            name))))
