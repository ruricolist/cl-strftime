(in-package #:cl-strftime)

(deftype universal-time ()
  'integer)

(deftype time ()
  '(or universal-time timestamp))

(deftype time-zone ()
  '(or (eql t) null local-time::timezone))

(defconst gregorian-cycle 400
  "Length of a cycle in the Gregorian calendar.")

(defconst year-months 12
  "Months in a year.")

(defconst month-days 31
  "Maximum number of days in a month.")

(defconst week-days 7
  "Days in a week.")

(defconst day-hours 24
  "Hours in a day.")

(defconst hour-seconds (* 60 60)
  "Seconds in an hour.")

(defconst am/pm-hours 12
  "Hours ante/post meridian.")

(defconst year-days 365
  "Days in a (non-leap) year.")

(defconst leap-year-days 366
  "Days in a leap year.")

(defconst century-years 100
  "Years in a century.")

(defconst zulu "Z")

(defconst gmt "GMT")

(defconst utc "UTC")

(deftype month ()
  "A month."
  '(integer 1 12))

(deftype day ()
  "A day of the month."
  '(integer 1 31))

(deftype week ()
  "A week of the year."
  '(integer 0 53))

(deftype iso-week ()
  '(integer 1 53))

(deftype weekday ()
  "A day of the week."
  '(integer 1 7))

(deftype hour ()
  "An hour."
  '(integer 1 24))

(deftype minute ()
  "A minute."
  '(integer 0 59))

(deftype second ()
  "A minute."
  '(integer 0 60))

(deftype year ()
  "A year."
  'integer)

(def months
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December")
  "Names of the months.")

(defun month-name (time)
  (aref months (1- (timestamp-month time))))

(def short-months
  (map 'vector (op (take 3 _)) months)
  "Abbreviated names of the months (for %b).")

(defun short-month-name (time)
  (aref short-months (1- (timestamp-month time))))

(def weekdays
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
  "Names of the days of the week.")

(defun weekday-name (time)
  (aref weekdays (timestamp-day-of-week time)))

(def short-weekdays
  (map 'vector (op (take 3 _)) weekdays)
  "Abbreviated names of the days of the week (for %a).")

(defun short-weekday-name (time)
  (aref short-weekdays (timestamp-day-of-week time)))

(def month-lengths
  #.(coerce #(31 28 31 30 31 30 31 31 30 31 30 31) '(vector fixnum)))

(def month-offsets
  (let ((diffs (scan #'+ month-lengths :initial-value 0)))
    (slice (coerce diffs '(vector fixnum)) 0 -1)))

(defun month-offset (m)
  (aref month-offsets (1- m)))

(def iso-long-year-table
  #.(loop with table = (make-array '(400)
                                   :element-type 'fixnum
                                   :initial-element 0)
          with iso-long-year-list =
          '(004 009 015 020 026 032 037 043 048 054 060 065 071 076 082
            088 093 099 105 111 116 122 128 133 139 144 150 156
            161 167 172 178 184 189 195 201 207 212 218 224 229 235 240
            246 252 257 263 268 274 280 285 291 296 303 308 314
            320 325 331 336 342 348 353 359 364 370 376 381 387 392 398)
          for i from 0 below 400
          if (member i iso-long-year-list)
            do (setf (aref table i) 1)
          finally (return table)))

(defun iso-long-year? (year)
  (eql 1 (aref iso-long-year-table (mod year 400))))

(defun last-week (year)
  (if (iso-long-year? year) 53 52))
