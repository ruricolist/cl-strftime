(in-package #:cl-strftime)

(defun format-time (stream format &optional (time (get-universal-time)) tz)
  "Write TIME to STREAM as instructed by FORMAT."
  (let ((formatter
          (if (functionp format)
              format
              (make-time-formatter format tz))))
    (if stream
        (funcall formatter stream :time time)
        (with-output-to-string (s)
          (funcall formatter s :time time)))))

(define-compiler-macro format-time (&whole decline stream format &optional time tz
                                           &environment env)
  (if (and (constantp format env) (constantp tz env))
      `(format-time ,stream (load-time-value (make-time-formatter ,format ,tz))
                    ,@(when time (list time)))
      decline))

(defun make-time-formatter (string tz)
  (let ((writers (nreverse (flatten! (compile-time-format string)))))
    (lambda (stream &key (time (get-universal-time)))
      (let ((time (make-universal-time time tz)))
        (dolist (fn writers)
          (funcall fn stream time))))))

(defun compile-time-format (string)
  (let (writers)
    (labels ((parse (&key (start 0))
               (multiple-value-bind (ms me rs re)
                   (ppcre:scan *directive-regex* string :start start)
                 (if ms
                     (progn
                       (push
                        (lambda (stream time)
                          (declare (ignore time))
                          (write-string string stream :start start :end ms))
                        writers)
                       (push (apply #'compile-directive
                                    (loop for s across rs
                                          for e across re
                                          collect (unless (= s e)
                                                    (subseq string s e))))
                             writers)
                       (parse :start me))
                     (push (lambda (stream time)
                             (declare (ignore time))
                             (write-string string stream :start start))
                           writers)))))
      (parse))
    writers))

(defparameter *directive-regex*
  (ppcre:create-scanner
   "%([-_0^#]?)([0-9]*)([a-zA-Z]|%)"
   :single-line-mode t
   :case-insensitive-mode nil))

(defun compile-directive (flag width directive)
  (let* ((action (cdr (assoc (character directive)
                             *time-directives*
                             :test #'char=)))
         (flag (if flag (character flag)
                   (when (consp action)
                     (second action))))
         (width (if width
                    (parse-integer width :junk-allowed t)
                    (when (consp action)
                      (first action)))))
    (cond ((null action)
           (error "~S is not a known directive" directive))
          ((characterp action)
           (lambda (stream time)
             (declare (ignore time))
             (write-char action stream)))
          ((stringp action)
           (compile-time-format action))
          (t (let ((fn (if (consp action) (third action) action)))
               (case flag
                 ;; Do not pad.
                 (#\-
                  (lambda (stream time)
                    (format stream "~A" (funcall fn time))))
                 ;; Pad with spaces.
                 (#\_
                  (lambda (stream time)
                    (format stream "~V,VD"
                            width #\Space
                            (funcall fn time))))
                 ;; Pad with zeros.
                 (#\0
                  (lambda (stream time)
                    (format stream "~V,VD"
                            width #\0
                            (funcall fn time))))
                 ;; Toggle case.
                 (#\#
                  (lambda (stream time)
                    (let ((value (funcall fn time)))
                      (format stream "~A"
                              (if (stringp value)
                                  (toggle-case value)
                                  value)))))
                 ;; Upcase.
                 (#\^
                  (lambda (stream time)
                    (format stream "~:@(~A~)"
                            (funcall fn time))))
                 (t
                  (lambda (stream time)
                    (format stream "~A" (funcall fn time))))))))))

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
