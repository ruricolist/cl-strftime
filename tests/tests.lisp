(in-package #:cl-strftime)

(defun sh (cmd)
  (uiop:run-program cmd
                    :force-shell t
                    :output :string
                    :directory *default-pathname-defaults*))

(defun date (format time)
  (slice (sh (fmt "date --date \'@~a\' +'~a'" time format)) 0 -1))

(defun random-unix-time ()
  (random-in-range 0 (get-unix-time)))

(defun a-unix-time ()
  "Generator for Unix times."
  #'random-unix-time)

(defun random-directive ()
  "Pick a directive at random."
  (random-elt (hash-table-keys time-directives)))

(defun a-format-string ()
  "Generator for format strings."
  (lambda (&optional (max-len 5))
    (let ((len (random-in-range 1 (1+ max-len))))
      (with-output-to-string (s)
        (loop for (d . rest) on (map-into (make-list len) #'random-directive)
              do (format s "%~a" d)
                 (when rest (format s " ")))))))

(defun one-of (xs)
  (let ((xs (coerce xs 'vector)))
    (lambda () (random-elt xs))))

(5am:def-suite cl-strftime)
(5am:in-suite cl-strftime)

(defun run-tests ()
  (5am:run! 'cl-strftime))

(defun test-output (string unix-time)
  (let ((cl (format-time nil string (unix-to-timestamp unix-time)))
        (date (date string unix-time)))
    (5am:is (equal cl date)
            "Discrepancy in time string:~%String: ~10t~a~%Time: ~10t~a~%Date: ~10t~s~%CL: ~10t~s"
            string unix-time date cl)))

(5am:test directives
  (5am:for-all ((unix-time (a-unix-time)))
    (dolist (d (remove #\v (hash-table-keys time-directives)))
      (let* ((s (fmt "%~a" d)))
        (test-output s unix-time)))))

(5am:test flags
  (5am:for-all ((unix-time (a-unix-time))
                (n (one-of '(nil 1 2 3)))
                (flag (one-of '(#\- #\_ #\0 #\# #\^))))
    (dolist (d (remove #\v (hash-table-keys time-directives)))
      (let* ((s (if n
                    (fmt "%~a~a~a" flag n d)
                    (fmt "%~a~a" flag d))))
        (test-output s unix-time)))))
