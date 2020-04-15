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
(print (scal-pro '(5 8 6) '(7 2 4)))
;----------------------------------------------------------------

;Задача 12
;Определите функцию, заменяющую в исходном списке два подряд идущих одинаковых элемента одним.
(defun 2to1 (lst) 
   ((lambda (lst1)
       (cond ((null lst) NIL)
        ((eq lst1 (cadr lst)) 
            (cons lst1 (2to1 (cddr lst)))
                    )
                   (t (cons lst1 (2to1 (cdr lst)))
                      )
            )
  )
  (car lst))
 )
(print (2to1 '(1 1 2 2 3 4)))
(print (2to1 '(3 3 3 4 5 5)))
(print (2to1 '(9 9 7 8 6 )))
;----------------------------------------------------------------

;Задача 9
;Определите функцию, разделяющую исходный список на два подсписка. Впервый из них должны попасть элементы с нечетными 
;номерами, во второй — элементы с четными номерами.
(defun sort (lst)
            (cond ((null lst) nil)
                (t ((lambda (lst1)
				(list (cons (car lst) (car lst1))
					        (cons (cadr lst) (cadr lst1))))
			(sort (cddr lst)))
        )
    )
)
(print (sort '(1 2 3 4 5 6)))
(print (sort '(10 5 3 7 2 2)))
(print (sort '(8 2 6 1 3 9)))
;----------------------------------------------------------------

;Задача 13
;Определите функцию, удаляющую в исходном списке все повторные вхождения элементов.

;----------------------------------------------------------------

;Задача 19
;Определите функцию (ЛУКОВИЦА n), строящую N-уровневый вложенный список, элементом которого на самом глубоком 
;уровне является N.
;----------------------------------------------------------------

;Задача 21
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.

;----------------------------------------------------------------

;Задача 27
;Определите функцию, которая, чередуя элементы списков(a b...) и (1 2...), образует новый список (a 1 b 2 ...).
(defun newlist (lst1 lst2)
        (cond ((null lst1) nil)
                ((null lst2) nil)
                 (t(cons(car lst1) 
                    (cons (car lst2) (newlist (cdr lst1) (cdr lst2)))
)
)
)
)
(print (newlist '(a b c d) '(1 2 3 4)))
(print (newlist '(b f c) '(2 4 5)))
;----------------------------------------------------------------

;Задача 33
;Определите функцию МНОЖЕСТВО, преобразующую список в множество.

;----------------------------------------------------------------

;Задача 39
;Определите функцию СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ, формирующую множество из элементов не входящих в оба множества.

;----------------------------------------------------------------

;Задача 45
;Предположим, что у имени города есть свойства х и у, которые содержат координаты места нахождения города относительно 
;некоторого начала координат. Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.

;----------------------------------------------------------------

;Задача 47
;Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.




