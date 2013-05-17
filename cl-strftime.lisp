(in-package #:cl-strftime)

(deftype index () '#.`(integer 0 ,array-dimension-limit))

(defun expand-tz (tz)
  (etypecase tz
    ((eql t) +utc-zone+)
    (null *default-timezone*)
    (local-time::timezone tz)))

(defun format-time (stream format &optional (time (now)) tz)
  "Write TIME to STREAM as instructed by FORMAT."
  (let ((time
          (etypecase time
            (timestamp time)
            (integer (universal-to-timestamp time))))
        (*default-timezone* (expand-tz tz)))
    (let ((formatter
            (if (functionp format)
                format
                (make-time-formatter format))))
      (if stream
          (funcall formatter stream :time time)
          (with-output-to-string (s)
            (funcall formatter s :time time))))))

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
  (let ((writers (nreverse (flatten! (compile-formatter format)))))
    (declare (optimize speed))
    (lambda (stream &key (time (now)))
      (let ((*default-timezone* (if force-gmt +utc-zone+ *default-timezone*)))
        (flet ((call/stream (stream)
                 (dolist (writer writers)
                   (declare (function writer) (optimize speed))
                   (funcall writer stream time))))
          (declare (inline call/stream))
          (if (streamp stream)
              (call/stream stream)
              (ecase stream
                ((t) (call/stream *standard-output*))
                ((nil) (with-output-to-string (s)
                         (call/stream s))))))))))

(defun make-time-formatter (format)
  (if (keywordp format)
      (ensure-gethash format
                      *compiled-time-formats*
                      (multiple-value-bind (f force-gmt)
                          (named-time-format format)
                        (make-time-formatter-1 f force-gmt)))
      (make-time-formatter-1 format nil)))

(defparameter *directive-regex*
  (ppcre:create-scanner
   "%([-_0^#]?)([0-9]*)([a-zA-Z]|%)"
   :single-line-mode t
   :case-insensitive-mode nil))

(defun compile-formatter (string)
  (declare (string string))
  (let (writers)
    (labels ((parse-from (start)
               (declare (optimize speed) (index start))
               (multiple-value-bind (ms me rs re)
                   (ppcre:scan *directive-regex* string :start start)
                 (if ms
                     ;; A directive.
                     (locally
                         (declare (index ms))
                       ;; Push a writer for the constant string before
                       ;; the directive.
                       (unless (= start ms)
                         (push
                          (lambda (stream time)
                            (declare (ignore time))
                            (write-string string stream :start start :end ms))
                          writers))
                       ;; Push a writer for the directive.
                       (push (apply #'compile-directive
                                    (loop for s of-type index across rs
                                          for e of-type index across re
                                          collect (unless (= s e)
                                                    (subseq string s e))))
                             writers)
                       (parse-from me))
                     ;; No more directives; push a writer for the
                     ;; constant remainder of the string.
                     (unless (= start (length string))
                       (push (lambda (stream time)
                               (declare (ignore time))
                               (write-string string stream :start start))
                             writers))))))
      (parse-from 0))
    ;; Don't reverse the writers yet; this might be a recursive call.
    writers))

(defun compile-flag (flag width action)
  (declare (function action) (optimize speed))
  (if (not flag)
      (lambda (s ts)
        (declare (stream s))
        (princ (funcall action ts) s))
      (ecase (character flag)
        ;; Do not pad.
        (#\-
         (lambda (s ts)
           (declare (stream s))
           (princ (funcall action ts) s)))
        ;; Pad with spaces.
        (#\_
         (lambda (s ts)
           (declare (stream s))
           (format s
                   (formatter "~V,VD")
                   width #\Space
                   (funcall action ts))))
        ;; Pad with zeros.
        (#\0
         (lambda (s ts)
           (declare (stream s))
           (format s
                   (formatter "~V,VD")
                   width #\0
                   (funcall action ts))))
        ;; Toggle case.
        (#\#
         (lambda (s ts)
           (declare (stream s))
           (let ((value (funcall action ts)))
             (if (stringp value)
                 (loop for c across value
                       do (cond ((not (both-case-p c))
                                 (write-char c s))
                                ((upper-case-p c)
                                 (write-char (char-downcase c) s))
                                ((lower-case-p c)
                                 (write-char (char-upcase c) s))))
                 (princ value s)))))
        ;; Upcase.
        (#\^
         (lambda (s ts)
           (declare (stream s))
           (format s
                   (formatter "~:@(~A~)")
                   (funcall action ts)))))))

(defun compile-directive (flag width directive)
  (multiple-value-bind (action default-width default-flag)
      (find-directive (character directive))
    (let ((flag (or flag default-flag))
          (width (or width default-width)))
      (etypecase action
        (null
         (error "~S is not a known directive" directive))
        (character
         (lambda (s ts)
           (declare (optimize speed) (stream s) (ignore ts))
           (write-char action s)))
        (string
         (compile-formatter action))
        (symbol
         (compile-flag flag width (symbol-function action)))))))
