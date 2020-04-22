;Задачи 9, 12, 13, 19, 21, 27, 33, 39, 45, 47
;----------------------------------------------------------------
;Задача 9
;Определите функцию, разделяющую исходный список на два подсписка. Впервый из них должны попасть элементы с нечетными 
;номерами, во второй — элементы с четными номерами.
(defun sep (lst)
        (cond ((NULL lst) NIL)
            (t ((lambda (lst1)
				(list
					(cons (car lst) (car lst1))
					(cons (cadr lst) (cadr lst1))))
			(sep (cddr lst)))
                    )
        )
)
(print (sep '(1 2 3 4 5 6)))
(print (sep '(10 5 3 7 2 2)))
(print (sep '(8 2 6 1 3 9)))
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
;Задача 13
;Определите функцию, удаляющую в исходном списке все повторные вхождения элементов.
(defun memblist(a lst)
	(cond ((NULL lst) NIL)
		((eq (car lst) a) t)
		(t (memblist a (cdr lst)))
	)
)
(defun remove-el(lst)
	(
		(lambda (a b)
			(cond ((NULL lst) NIL)
				(
					(memblist a b)
					(remove-el b)
				)
				(t (cons a (remove-el b))
				)
			)
		)
		(car lst)(cdr lst)
	)
)

(print(remove-el '(1 2 2 3 3 3)))
(print(remove-el '(4 4 4 4 3 5 5 5)))
(print(remove-el '(8 1 1 1 1)))

;----------------------------------------------------------------

;Задача 19
;Определите функцию (ЛУКОВИЦА n), строящую N-уровневый вложенный список, элементом которого на самом глубоком 
;уровне является N.
(defun onion (n &optional (k n))
  (cond ((zerop k) n)
        (t (list (onion n (1- k))))))
(print (onion 3))
(print (onion 8))
(print (onion 5))
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
(defun list1(elem lst)
	(cond((NULL lst) NIL)
		((eq elem (car lst))t)
		(t (list1 elem (cdr lst)))	
	)
)

(defun setlist (lst)
	((lambda(l1 l2)
		(cond ((NULL lst) NIL)
			((list1 l1 l2) (setlist l2))
			(t (cons l1 (setlist l2)))
		)
	)
  (car lst) (cdr lst))	
)
(print (setlist '(1 1 3 4 7 1 4 6 8)))
(print (setlist '(3 4 7 3 4 9 9 8)))
(print (setlist '(2 2 2 3 4 7 8 4 6)))
;----------------------------------------------------------------

;Задача 39
;Определите функцию СИММЕТРИЧЕСКАЯ-РАЗНОСТЬ, формирующую множество из элементов не входящих в оба множества.

;----------------------------------------------------------------

;Задача 45
;Предположим, что у имени города есть свойства х и у, которые содержат координаты места нахождения города относительно 
;некоторого начала координат. Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами а и b.
(defun setc(cityname x y)
	(setf (get cityname 'x) x)
	(setf (get cityname 'y) y)
)
(defun pow(x)
	(* x x))

(defun dist(c1 c2)
	(sqrt (+ (pow (- (get c1 'x) (get c2 'x))) 
		 (pow (- (get c1 'y) (get c2 'y)))
		)
	)
)

(defun dist1(city1 city2 x1 y1 x2 y2)
	(setc city1 x1 y1)
	(setc city2 x2 y2)
	(dist city1 city2)
)
(print (dist1 'City1 'City2 0 1 12 10))
(print (dist1 'Simferopol 'Dzhankoy 1 1 25 25))
(print (dist1 'Yalta 'Dzhankoy 0 0 45 45))

;----------------------------------------------------------------
;Задача 47
;Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.
(defun del-prop(x)
	((lambda(prop-list)
		(cond ((NULL prop-list) t)
			(t (remprop x (car prop-list))(del-prop x)) 	
		)
	)(symbol-plist x))	 
) 
(setq x 'Desktop) 
(setf (get 'Desktop 'brand) 'Toshiba) 
(setf (get 'Desktop 'color) 'grey) 
(setf (get 'Desktop 'year) '2013)

(print (symbol-plist 'Desktop))
(del-prop 'Desktop) 
(print (symbol-plist 'Desktop))
