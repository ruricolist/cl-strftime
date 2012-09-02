(in-package #:cl-strftime)

(defun format-time (stream format &optional (time (now)) tz)
  "Write TIME to STREAM as instructed by FORMAT."
  (let ((time
          (etypecase time
            (timestamp time)
            (integer (universal-to-timestamp time))))
        (tz
          (etypecase tz
            ((eql t) +utc-zone+)
            (null *default-timezone*)
            (local-time::timezone tz))))
    (let ((formatter
            (if (functionp format)
                format
                (make-time-formatter format tz))))
      (if stream
          (funcall formatter stream :time time)
          (with-output-to-string (s)
            (funcall formatter s :time time))))))

(define-compiler-macro format-time (&whole decline stream format &optional time (tz *default-timezone*)
                                           &environment env)
  (if (and (constantp format env) (constantp tz env))
      `(format-time ,stream (load-time-value (make-time-formatter ,format ,tz))
                    ,@(when time (list time)))
      decline))

(defmacro writer ((stream time) &body body)
  `(lambda (,stream ,time)
     (declare (stream ,stream) (timestamp ,time)
              (optimize speed) (ignorable ,stream ,time))
     ,@body))

(defun make-time-formatter (format tz)
  (if (keywordp format)
      (multiple-value-bind (f gmt)
          (named-time-format format)
        (setf format f)
        (if gmt (setf tz +utc-zone+))))
  (let ((writers (nreverse (flatten! (compile-time-format format)))))
    (lambda (stream &key (time (now)))
      (labels ((call/stream (stream)
                 (dolist (writer writers)
                   (declare (function writer)
                            (optimize speed))
                   (funcall writer stream time))))
        (declare (inline call/stream))
        (if (streamp stream)
            (call/stream stream)
            (ecase stream
              ((t) (call/stream *standard-output*))
              ((nil) (with-output-to-string (s)
                       (call/stream s)))))))))

(defun compile-time-format (string)
  (let (writers)
    (labels ((parse (&key (start 0))
               (multiple-value-bind (ms me rs re)
                   (ppcre:scan *directive-regex* string :start start)
                 (if ms
                     (progn
                       (push
                        (writer (stream time)
                          (write-string string stream :start start :end ms))
                        writers)
                       (push (apply #'compile-directive
                                    (loop for s across rs
                                          for e across re
                                          collect (unless (= s e)
                                                    (subseq string s e))))
                             writers)
                       (parse :start me))
                     (push (writer (stream time)
                             (write-string string stream :start start))
                           writers)))))
      (parse))
    writers))

(defparameter *directive-regex*
  (ppcre:create-scanner
   "%([-_0^#]?)([0-9]*)([a-zA-Z]|%)"
   :single-line-mode t
   :case-insensitive-mode nil))

(defun compile-flag (flag width action)
  (declare (function action))
  (case flag
    ;; Do not pad.
    (#\-
     (writer (s ts)
       (format s "~A" (funcall action ts))))
    ;; Pad with spaces.
    (#\_
     (writer (s ts)
       (format s "~V,VD" width #\Space (funcall action ts))))
    ;; Pad with zeros.
    (#\0
     (writer (s ts)
       (format s "~V,VD"
               width #\0
               (funcall action ts))))
    ;; Toggle case.
    (#\#
     (writer (s ts)
       (let ((value (funcall action ts)))
         (format s "~A"
                 (if (stringp value)
                     (toggle-case value)
                     value)))))
    ;; Upcase.
    (#\^
     (writer (s ts)
       (format s "~:@(~A~)"
               (funcall action ts))))
    (t
     (writer (s ts)
       (format s "~A" (funcall action ts))))))

(defun compile-directive (flag width directive)
  (multiple-value-bind (action default-flag default-width)
      (find-directive (character directive))
    (let ((flag (or flag default-flag))
          (width (or width default-width)))
      (etypecase action
        (null
         (error "~S is not a known directive" directive))
        (character
         (writer (s ts)
           (write-char action s)))
        (string
         (compile-time-format action))
        (symbol
         (compile-flag flag width (symbol-function action)))))))

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
