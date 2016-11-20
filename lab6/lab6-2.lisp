; Дана информация о телефонных разговорах сотрудников некоторой компании в виде списка CDR-записей
; (Call Detail Record). Каждая запись представляет собой строку, содержащую сведения об одном звонке.
; Формат записи следующий: "Номер-А;Номер-Б;Длительность-разговора". "Номер А" – это номер
; вызывающего абонента (то есть номер того, кто звонил), "Номер Б" – номер вызываемого абонента
; (то есть кому звонили), "Длительность-разговора" - длительность разговорного состояния в секундах.
; Разделитель полей – точка с запятой ';', между полей могут присутствовать пробелы (но не внутри
; полей). Записи другого формата считать некорректными и игнорировать.
;
; Внутренние номера сотрудников компании – четырехзначные и начинаются с единицы, например, "1101",
; "1299" и т.д. Внешние номера записаны в соответствии с общим международным телекоммуникационным
; планом нумерации E.164, то есть начинаются с символа '+', за которым следует не более 15 цифр: код
; страны и телефонный номер, например "+78129991316" (7 - Россия, 812 - Санкт-Петербург,
; 999-13-16 – номер) или "+3314562024112" (33 - Франция, 1 - Париж, 45 62 02 41 12 - номер).
;
; Необходимо прочитать список CDR-записей из стандартного потока ввода и определить, какова общая
; длительность звонков, поступивших сотруднику компании (входящих звонков), который чаще всего звонил
; в Бразилию (код страны 55). В стандартный поток вывода записать общую длительность поступивших этому
; сотруднику входящих звонков.
;
; Пример:
; ("1101;+79119989911;122" " 1102;+55231114563; 347" "+55123334521;1101;134" "1102;+55023334521;811" "ERR01:1234;;;0;0" "1101; +550145211212; 93" "1101;+47023414522;753" "1102;1101;42" "+47023414522;1102; 62" "+79119989911;1102;771")

(defparameter lst (read))

;;;;=========
(defun hash++ (h key &optional (n 0))
   (let ((val (gethash key h)))
       (cond
           ((or (null val) (not (numberp val)))
               (set% h key n))
           (t (set% h key (+ val n))))
       (gethash key h)))

(defun set% (h key val &rest other-pairs)
   (let ((pairs (append (list key val) other-pairs)) k v)
       (hash-table-add-list h pairs)))

(defun hash-table-add-list (h lst &aux k v)
   (loop
       (if (null lst) (return)) (setf k (pop lst))
       (if (null lst) (return)) (setf v (pop lst))
       (setf (gethash k h) v))
   h)

(defun hash-table-key-value-pairs (h)
    (let (pairs)
        (maphash #'(lambda (k v)
            (push (list k v) pairs)) h)
        (nreverse pairs)))
;;;;============

(defun outer-start-pos (str)
    (let ((p (position #\+ str)))
        (if p
            (1+ p)
            nil)))

(defun outer-end-pos (str)
    (let ((start (outer-start-pos str)))
        (if start
            (position #\; str :start start)
            nil)
        ))

(defun get-outer-num (str)
    (let (  (start (outer-start-pos str))
            (end (outer-end-pos str)))
        (if (and
                (numberp start)
                (numberp end)
                (> end start))
            (string-trim " " (subseq str start end))
            nil)))

(defun alldigit-p (str)
    (numberp (read-from-string str)))

(defun internal-num-p (num)
    (and
        (= (length num) 4)
        (alldigit-p num)
        (char= #\1 (elt num 0))))

(defun get-internal-num (str)
    (string-trim " " (subseq str 0 (position #\; str))))

(defun outer-num-p (num)
    (and
        num
        (<= (length num) 15)
        (alldigit-p num)))

(defun bz-num-p (num)
    (and
        (char= #\5 (elt num 0))
        (char= #\5 (elt num 1))))

(defun internal-bz-call-p (str)
    (let (  (numa (get-internal-num str))
            (numb (get-outer-num str)))
        (and
            (internal-num-p numa)
            (outer-num-p numb)
            (bz-num-p numb))))

(defun get-duration (str)
    (read-from-string (string-trim " "(subseq str (1+ (position #\; str :from-end t))))))


(defun freq-bz-calls (lst)
    (sort
        (hash-table-key-value-pairs
            (reduce
                #'(lambda (h str)
                    (hash++ h (get-internal-num str) (get-duration str))
                    h)
                (remove-if-not
                    #'internal-bz-call-p
                    lst)
                :initial-value (make-hash-table :test #'equal)))
        #'> :key #'second))

(defun callee-num (str)
    (string-trim " "
        (subseq str (1+ (position #\; str)) (position #\; str :from-end t))))

(defun callee-hash (lst)
    (reduce
        #'(lambda (h str)
            (hash++ h (callee-num str) (get-duration str))
            h)
        lst
        :initial-value (make-hash-table :test #'equal)))

(defun duration-for-callee-by-max-bz-outgoing (lst)
    (let (  (*out-bz-num* (caar (freq-bz-calls lst)))
            (*callee-ht* (callee-hash lst)))
        (gethash *out-bz-num* *callee-ht*)))

(print (duration-for-callee-by-max-bz-outgoing lst))
