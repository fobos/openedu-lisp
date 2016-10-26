; Прочитать трехмерный массив, содержащий строки, из стандартного потока ввода.
; Необходимо определить, какая строка встречается в массиве чаще всего, и вывести на экран
; количество вхождений.
;
; Пример:
; #3A((("R" "Lisp" "Python" "perl") ("C" "Lisp" "Pascal" "D")) (("SQL" "Java" "Pascal" "Lisp") ("Nemerle" "R" "Julia" "piet")) (("C++" "Oberon" "C" "Pascal") ("Lisp" "Python" "Ruby" "FORTRAN")))


(defparameter arr (read))

(defvar lst '())
(defvar dim (array-dimensions arr))

(defvar l nil)
(defun push-assoc (key)
    (setf l (assoc key lst :test #'string=))
    (if (not l)
        (push (cons key 1) lst)
        (setf l (incf (cdr l)))))


(dotimes (i (first dim))
    (dotimes (j (second dim))
        (dotimes (k (third dim))
            (push-assoc (aref arr i j k)))))

(print lst)

(defvar times 0)
(loop for x in lst
    do (if (> (cdr x) times)
        (setf times (cdr x))))

(print times)
