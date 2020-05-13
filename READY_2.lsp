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
(defun НЕКОТОРЫЙ (пред список)
       (mapcan #'(lambda (x) (funcall пред x)) список))


(print (НЕКОТОРЫЙ 'atom '(a b c)))
(print (НЕКОТОРЫЙ 'zerop '(1 2 3)))
;----------------------------------------------------------------

;Задача 7 
;Определите фильтр (УДАЛйЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
;все элементы, которые не обладают свойством, наличие которого проверяет
;предикат пред.
(defun УДАЛИТЬ-ЕСЛИ-НЕ (пред список)
	(mapcan #'(lambda (x) 
	        (if (funcall пред x) (list x) NIL)) список)) 
 
(print (УДАЛИТЬ-ЕСЛИ-НЕ #'zerop '(1 0 3 0 5 0 7)))
(print (УДАЛИТЬ-ЕСЛИ-НЕ #'oddp '(1 2 3 4 5 6 7)))
;----------------------------------------------------------------

;Задача 11 
;Определите фукнционал МНОГОФУН, который использует функции, являющиеся
;аргументами, по следующей схеме:
;(МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).
(defun МНОГОФУН (список x)
  (mapcar #'(lambda (f) (funcall f x)) список))
  
(print (МНОГОФУН '(+ - * /) 5))
(print (МНОГОФУН '(sin cos abs) 9))
;----------------------------------------------------------------

;Задача 13
;Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).


