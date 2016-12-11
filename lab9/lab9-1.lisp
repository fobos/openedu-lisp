; Необходимо сделать так, чтобы последовательность #[A B] воспринималась Лиспом как список простых
; чисел из заданного диапазона, где A – целое неотрицательное число, с которого начинается диапазон,
; B – целое неотрицательное число, которым заканчивается диапазон. В случае ошибок должен
; возвращаться пустой список (то есть значение nil).
;
; Пример:
; В результате вычисления формы (defparameter primes-list-1 #[1 11]) переменная primes-list-1 должна
; содержать список (2 3 5 7 11).
; В результате вычисления формы (defparameter primes-list-2 #[5 -5]) переменная primes-list-2 должна
; быть равна nil.


(set-macro-character #\] (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
    #'(lambda (stream c1 c2)
        (declare (ignore c1 c2))
        (let* (  (lst (read-delimited-list #\] stream t))
                (begin (first lst))
                (end (second lst)))
            (if (and
                    (= (list-length lst) 2)
                    (>= begin 0)
                    (> end begin))
                `(quote
                    ,(loop for x from begin to end
                        when (primep x) collect x)
                )))))

(defun primep (x)
    (= (- x 1) (mod (factr (- x 1) 1) x)))

(defun factr (n &optional (res 1))
    (if (or (= n 0) (= n 1))
        res
        (factr (- n 1) (* res n))))


(print #[1 11])
