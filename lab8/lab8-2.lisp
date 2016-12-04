; Напишите функцию format+, которая поддерживает все возможности функции format, но вдобавок
; позволяет обрабатывать форматную директиву ~z, предназначенную для форматирования времени.
;
; Временная отметка должна форматироваться следующим образом: HH:MM:SS.mmm, где
; НН – часы,
; MM – минуты,
; SS – секунды,
; mmm – миллисекунды.
; Если часы, минуты или секунды состоят из одной цифры, то они дополняются до двух нулем.
; Поле миллисекунд состоит из трех цифр.
;
; Аргументом директивы ~z должен выступать список из четырех элементов (HH MM SS mmm), первые три
; из которых могут быть либо целым положительным числом, либо строкой, а последний – вещественным
; числом (у которого количество цифр после запятой может быть больше трех). Если какой-либо из
; элементов списка равен nil, временная отметка считается неправильной. Если какие-либо компоненты
; имеют недопустимые значения (например, минуты – 90), временная отметка считается неправильной.
; В этом случае вместо нее вывести строку ??:??:??.000
;
; Примеры форматирования с помощью директивы ~z:
; (format+ t "~,3f: time ~z" 0.5 '(0 "1" 22 0.23113)) ==> 0.500: time 00:01:22.231
; (format+ nil "~z" '(25 10 10 0.5)) ==> "??:??:??.000"

(defun parse-num (num)
    (cond ((stringp num) (parse-integer num))
          ((integerp num) num)
          (t -1)))

(defun parse-float (f)
    (if (floatp f) f 0))

(defun process-timelist (lst)
    (let (  (hh (parse-num (first lst)))
            (mm (parse-num (second lst)))
            (ss (parse-num (third lst)))
            (mmm (parse-float (fourth lst))))
        (if (and
                (>= hh 0)
                (< hh 24)
                (>= mm 0)
                (< mm 60)
                (>= ss 0)
                (< ss 60))
            (values hh mm ss mmm)
            (values-list '("??" "??" "??" 0)))))

(defun fmtz (zargs)
    (multiple-value-bind
        (hh mm ss mmm)
            (process-timelist zargs)
            (format nil "~2,'0d:~2,'0d:~2,'0d~4,3f" hh mm ss mmm)))

(defun format+ (*out* fstr &rest args)
    (let (
            (new-args (mapcar
                    #'(lambda (arg)
                        (if (listp arg) (fmtz arg) arg))
                    args))
            (new-fstr (substitute #\a #\z fstr)))
        (apply #'format *out* new-fstr new-args)))

; (format+ t "~,3f: time ~z" 0.5 '(0 "1" 22 0.23113))
; (format+ nil "~z" '(25 10 10 0.5))
