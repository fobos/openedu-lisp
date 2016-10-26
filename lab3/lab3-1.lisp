; Прочитать трехмерный массив из стандартного потока ввода. Массив может содержать элементы
; различных типов – целые, вещественные, комплексные числа, строки, символы, списки, другие массивы.
; Необходимо найти сумму целых чисел, не делящихся нацело на 5. Если целых чисел в массиве нет,
; результат – 0.

; Пример:
; Дан трехмерный массив
; #3A(((1.8 5 #\B "may") ("the" 2.25 1.3 4) (17 0 "force" 55)) (("be" #C(0 1) 5.02 99) ("with" 1.1 (#\Y #\N) 0.87) ("you!" #C(2.2 3.3) 42 4.2)))

(defparameter arr (read))

(setf res 0)

(defun parse-elem (elem)
    (when (integerp elem)
        (calc elem))
    (when (arrayp elem)
        (unless (stringp elem)
            (dotimes (i (array-total-size elem))
                (calc (aref elem i)))))
    (when (listp elem)
        (loop for x in elem
            do (calc x))))

(defun calc (elem)
    (if (and
            (integerp elem)
            (/= (rem elem 5) 0))
        (setf res (+ res elem))))

(dotimes (i 2)
    (dotimes (j 3)
        (dotimes (k 4)
            (parse-elem (aref arr i j k)))))

(print res)
