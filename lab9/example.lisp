(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[ #'(lambda (stream c1 c2)
    (declare (ignore c1 c2))
    (let ((lst (read-delimited-list #\] stream t)))
        `(quote
            ,(loop for x from (first lst) to (second lst)
                by (or (third lst) 1) collect x)))))

(print #[1 10 2])
