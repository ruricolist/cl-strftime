(in-package #:cl-strftime)

(def named-time-formats
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

(defun named-time-format (name &optional gmt?)
  (check-type name keyword)
  (multiple-value-bind (format format?)
      (gethash name named-time-formats)
    (if (not format?)
        (error 'unknown-format :name name)
        (destructuring-bind (format &key ((:gmt gmt?) gmt?)) format
          (if (symbolp format)
              (named-time-format format gmt?)
              (values format gmt?))))))
