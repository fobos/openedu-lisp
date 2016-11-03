; Прочитать из стандартного потока ввода двоичное дерево, представленное с помощью списков.
; Первый элемент в списке является значением узла, второй элемент – левым поддеревом (списком) или
; значением nil, если у данного узла нет левого потомка, третий элемент – правым поддеревом (списком)
; или значением nil, если у данного узла нет правого потомка. В качестве значения в узле могут
; храниться любые данные, в том числе целые числа. Найти сумму кратных 5 целых чисел, хранящихся
; в узлах дерева, расположенных на уровнях с четными номерами. Уровень корня принимается равным 1.
; Если в дереве нет удовлетворяющих условиям задачи узлов, считать сумму равной 0.
;
; Пример:
; Дано дерево (1 (10 (15 nil nil) (7 nil nil)) (5 (2 nil nil) (8 nil nil)))

(defparameter lst (read))

(defun left (tree) (second tree))
(defun right (tree) (third tree))

(defvar res 0)
(defun bst-levelorder (tree)
    (labels ((do-bst-levelorder (nodes &optional (level 1) &aux lst)
        (dolist (node nodes)
            (unless (null node)
                (let ((val (first node)))
                    (if (and (evenp level) (= (rem val 5) 0))
                        (setf res (+ res val))))

                (if (left node)
                    (setf lst
                        (append lst (list (left node)))))
                (if (right node)
                    (setf lst
                        (append lst (list (right node)))))))
        (if lst (do-bst-levelorder lst (incf level)))))

        (do-bst-levelorder (list tree))))

(bst-levelorder lst)

(print res)
