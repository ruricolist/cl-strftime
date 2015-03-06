(in-package #:cl-strftime)

(defvar *default-width* nil)

(deftype writer ()
  '(or string function))

(defun expand-tz (tz)
  (etypecase-of time-zone tz
    ((eql t) +utc-zone+)
    (null *default-timezone*)
    (local-time::timezone tz)))

(defun format-time (stream format &optional (time (now)) tz)
  "Write TIME to STREAM as instructed by FORMAT.

TZ may be `nil' (for the current default timezone), `t' (for GMT), or
an object of type `local-time::timezone'.

FLAGS

- Do not pad
_ Pad with spaces
0 Pad with zeros
# Toggle case
^ Upcase

DIRECTIVES

%% Literal percent sign
%A Weekday (Sunday)
%a Abbreviated weekday (Sun)
%B Month (January)
%b Abbreviated month (Jan)
%C Century (20)
%d Day of month (01-31)
%D Same as %m/%d/%y
%e Same as %_2d
%F Same as %Y-%m-%d
%G ISO 8601 year
%g ISO 8601 year in century
%h Same as %b
%H Hour (01–24)
%I Hour (01–12)
%j Day of year (001-366)
%k Hour ( 1-24)
%l Hour ( 1-12)
%m Month (01-12)
%M Minute (00-59)
%n Literal newline
%p AM or PM
%P am or pm
%r Same as %I:%M:%S %p
%R Same as %H:%M
%s Epoch time
%S Second (00-59)
%t Literal tab
%T Same as %H:%M:%S
%u Day of week (weeks start on Monday)
%U Week number
%V ISO 8601 week number
%w Day of week
%W Week number (weeks start on Monday)
%y Year of century (12)
%Y Year (2012)
%z RFC 2822 timezone
%Z Name of timezone

NAMED FORMATS

\(Formats on the same line are synonyms.)

:ARPA :EMAIL :RFC-822
:ASCTIME :CTIME
:COOKIE
:HTTP :RFC-2616
:NEWS :RFC-850 :USENET
:RSS :RFC-2822 :RFC-1036 :RFC-2086
:RFC-1132
:UNIX
:W3C :HTML :RFC-3339 :XML :ATOM
"
  (let ((time
          (etypecase-of time time
            (timestamp time)
            (universal-time (universal-to-timestamp time))))
        (*default-timezone* (expand-tz tz))
        (formatter
          (if (functionp format)
              format
              (make-time-formatter format))))
    (with-string (stream stream)
      (funcall formatter stream time))))

(define-compiler-macro format-time (&whole decline
                                           stream format
                                           &optional time tz
                                           &environment env)
  (if (constantp format env)
      `(format-time ,stream (load-time-value (make-time-formatter ,format))
                    ,@(when time (list time)) ,@(when tz (list tz)))
      decline))

(defun flatten! (list)
  (declare (optimize speed))
  (labels ((flatten! (list)
             (mapcan
              (lambda (elt)
                (if (atom elt)
                    (list elt)
                    (flatten! elt)))
              list)))
    (flatten! list)))

(defvar *compiled-time-formats* (make-hash-table))

(defun make-time-formatter-1 (format force-gmt)
  (let ((writers (flatten! (compile-formatter format))))
    (lambda (stream time) (declare (optimize speed))
      (let ((*default-timezone* (if force-gmt +utc-zone+ *default-timezone*)))
        (dolist (writer writers) (declare (function writer))
          (funcall writer stream time))))))

(defun make-time-formatter (format)
  (if (keywordp format)
      (multiple-value-bind (f force-gmt)
          (named-time-format format)
        (make-time-formatter-1 f force-gmt))
      (make-time-formatter-1 format nil)))

(defun parse-format-string (string)
  (nlet parse ((start 0)
               (acc ()))
    (declare (array-index start))
    (multiple-value-bind (ms me rs re)
        (ppcre:scan "%([-_0^#]?)(\\d*)([a-zA-Z%])" string :start start)
      (if (null ms)
          (if (= start (length string))
              (nreverse acc)
              (let ((tail (subseq string start)))
                (nreverse (cons tail acc))))
          (let ((directive
                  (loop for s across rs
                        for e across re
                        collect (if (= s e)
                                    nil
                                    (subseq string s e)))))
            (if (= start ms)
                (parse me (cons directive acc))
                (let ((prefix (subseq string start ms)))
                  (parse me (list* directive prefix acc)))))))))

(defun compile-parsed-format (format)
  (mapcar (lambda (part)
            (etypecase part
              (string
               (lambda (stream time) (declare (ignore time))
                 (write-string part stream)))
              (list (apply #'compile-directive part))))
          format))

(defun compile-formatter (string)
  (compile-parsed-format (parse-format-string string)))

(defun compile-flag-char (flag-char width action)
  (let ((action (ensure-function action)))
    (ecase-of flag flag-char
      ;; Don't pad.
      (#\-
       (lambda (s ts)
         (declare (stream s))
         (princ (funcall action ts) s)))
      ;; Pad with spaces.
      (#\_
       (lambda (s ts) (declare (stream s))
         (format s "~v,1,0,' @a" width (funcall action ts))))
      ;; Pad with zeros.
      (#\0
       (lambda (s ts) (declare (stream s))
         (format s "~v,1,0,'0@a" width (funcall action ts))))
      ;; Toggle case.
      (#\#
       (lambda (s ts) (declare (stream s))
         (let ((value (funcall action ts)))
           (if (stringp value)
               ;; E.g. AM, CDT.
               (loop for c across value
                     do (cond ((upper-case-p c)
                               (write-char (char-downcase c) s))
                              ((lower-case-p c)
                               (write-char (char-upcase c) s))
                              (t (write-char c s))))
               (princ value s)))))
      ;; Upcase.
      (#\^
       (if width
           (compile-flag-char #\_ width
                              (lambda (ts)
                                (fmt "~:@(~A~)" (funcall action ts))))
           (lambda (s ts) (declare (stream s))
             (format s "~:@(~A~)" (funcall action ts))))))))

(defun compile-flag (flag width action)
  (declare (function action) (optimize speed))
  (check-type width (or integer null))
  (if (not flag)
      (lambda (s ts)
        (declare (stream s))
        (princ (funcall action ts) s))
      (compile-flag-char (character flag) width action)))

(defun compile-directive (flag width directive)
  (when width (setf width (parse-integer width)))
  (multiple-value-bind (action default-width default-flag)
      (find-directive (character directive))
    (let* ((flag (or flag default-flag))
           (width (or width default-width))
           ;; Cannot specify a /smaller/ width.
           (width (if width
                      (if default-width
                          (max width default-width)
                          (max 0 width))
                      width)))
      (etypecase-of directive-action action
        (character
         (if width
             ;; "%02t" -> "0	"
             (let ((string (string action)))
               (compile-flag flag width (constantly string)))
             (lambda (s ts) (declare (stream s) (ignore ts))
               (write-char action s))))
        (string
         (compile-formatter action))
        (symbol
         (compile-flag flag width (symbol-function action)))))))
