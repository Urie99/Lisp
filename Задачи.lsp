;Задачи 13, 9, 12, 15, 19, 21, 27, 33, 39, 45, 47
;----------------------------------------------------------------

;Задача 15
;Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.
(defun scal-pro (lst1 lst2) 
	(cond ((null lst1) 0)
		(t (+ (* (car lst1) (car lst2)) (scal-pro (cdr lst1) (cdr lst2))))
	)
)
(print (scal-pro '(1 2 3) '(3 2 1)))

;Тесты
;(product `(1 2 3) `(3 2 1))
;10
;(product `(5 8 6) `(7 2 4))
;75
;----------------------------------------------------------------

;Задача 12
;Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.
