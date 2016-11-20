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
; Необходимо прочитать список CDR-записей из стандартного потока ввода и определить, суммарная
; длительность всех исходящих звонков в Германию (код страны 49) и Италию (код страны 39) кого из
; сотрудников компании (то есть с какого внутреннего номера) была максимальной. В стандартный поток
; вывода записать внутренний номер сотрудника.
;
; Пример:
; ("1101;+79119989911;122" "+49231114563;1102;347" "1101;+420023334521;134" "1102;+49023334521;811" "ERR01:1234;;;0;0" "1101;+390145211212;93" "1101;+49023334521;756")

(defparameter lst (read))

(defun numa-end-pos (str)
    (position #\; str))

(defun numb-start-pos (str)
    (let ((p (position #\+ str)))
        (if p
            (1+ p)
            nil)))

(defun numb-end-pos (str)
    (let ((start (numb-start-pos str)))
        (if start
            (position #\; str :start start)
            nil)
        ))

(defun alldigit-p (str)
    (numberp (read-from-string str)))


(defun get-numa (str)
    (subseq str 0 (numa-end-pos str)))

(defun get-numb (str)
    (let ((start (numb-start-pos str)) (end (numb-end-pos str)))
        (if (and
                (numberp start)
                (numberp end)
                (> end start))
            (subseq str start end)
            nil)))

(defun get-duration (str)
    (let ((pos (numb-end-pos str)))
        (if (numberp pos)
            (subseq str (1+ pos))
            nil)))

(defun parse-call (call)
    (let (  (numa (get-numa call))
            (numb (get-numb call))
            (duration (get-duration call)))
        (list numa numb duration)))

(defun numa-p (numa)
    (and
        (= (length numa) 4)
        (alldigit-p numa)
        (char= #\1 (elt numa 0))))

(defun numb-p (numb)
    (and
        (<= (length numb) 15)
        (alldigit-p numb)))

(defun duration-p (duration)
    (alldigit-p duration))

(defun valid-call-p (l)
    (and
        (numa-p (first l))
        (numb-p (second l))
        (duration-p (third l))))

(defun max-ge-it-calls (lst)
    (remove-if-not
        #'valid-call-p
        (mapcar #'parse-call lst)))


(print (max-ge-it-calls lst))

; (print
;     (mapcar #'numa-end-pos lst))
;
; (print
;     (mapcar #'numb-start-pos lst))
;
;     (print
;         (mapcar #'numb-end-pos lst))
