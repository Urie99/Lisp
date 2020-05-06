;Функции высших порядков
;Задача 1
;Определите FUNCALL через функционал APPLY.
(defun FUN-CALL (func &rest args) 
              (APPLY func args))

(print (FUNCALL '+ 1 2 3 4))
(print (FUN-CALL '+ 1 2 3 4))
;----------------------------------------------------------------

;Задача 3
;Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка
;(f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn)
;и возвращает список, сформированный из результатов.
(defun APL-APPLY (func-lst elements)
  (cond ((NULL func-lst) NIL) 
        ((NULL elements) NIL)
    (t (cons (FUNCALL (CAR func-lst) (CAR elements))
             (APL-APPLY (CDR func-lst) (CDR elements))))))
             
(setq func-lst '(list cdr car))
(setq elements '((a b c) (1 2 3) (A B C)))
(print (APL-APPLY func-lst elements))
;----------------------------------------------------------------

;Задача 5
;Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, 
;когда, являющейся функциональным аргументом предикат пред истинен хотя бы для 
;одного элемента списка список.
