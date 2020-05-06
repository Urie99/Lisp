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