; Напишите макрос do-primes, который позволяет перечислить все простые числа из заданного диапазона.
; Макрос должен иметь следующий вид:
;
; (defmacro do-primes ((var startv endv) &body body)
; ;; код макроса
; )
;
;
; Аргумент var – имя переменной, которая на каждой итерации принимает значение очередного простого числа;
; аргумент startv – значение, с которого начинается проверка (начало диапазона);
; endv – значение, на котором заканчивается проверка (конец диапазона).
;
; Пример: Вызов (do-primes (n 3 11) (fresh-line) (prinс n)) должен вывести на экран следующие числа:
; 3
; 5
; 7
; 11

(defmacro do-primes ((var startv endv) &body body)
    `(loop for ,var from ,startv to ,endv
        when (primep ,var) do ,@body)
)

(defun primep (x)
    (= (- x 1) (mod (factr (- x 1) 1) x)))

(defun factr (n &optional (res 1))
    (if (or (= n 0) (= n 1))
        res
        (factr (- n 1) (* res n))))
