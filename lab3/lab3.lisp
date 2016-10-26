; Прочитать трехмерный массив, содержащий вещественные числа, из стандартного потока ввода.
; Необходимо определить, какое число встречается в массиве чаще всего,
; и вывести на экран количество вхождений.
; #3A(((3.32 1.1 4.2 0.5) (7.7 3.33 1.1 0.5)) ((7.7 4.2 9.6 7.1) (12.12 0.03 4.2 0.03)) ((4.2 11.2 0.11 0.5) (8.03 4.2 1.1 2.77)))


(defparameter arr (read))

(setf lst '())
;(setf dim (array-dimensions arr))

(defun push-assoc (key)
    (setf l (assoc key lst))
    (if (not l)
        (push (cons key 1) lst)
        (setf l (incf (cdr l)))))


(dotimes (i 3)
    (dotimes (j 2)
        (dotimes (k 4)
            (push-assoc (aref arr i j k)))))

(setf times 0)
(loop for x in lst
    do (if (> (cdr x) times)
        (setf times (cdr x))))

(print times)
