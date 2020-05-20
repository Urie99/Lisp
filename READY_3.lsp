;Макросы
;Задача 1
;Определите макрос, который возвращает свой вызов.
(defmacro call (&rest self)
   `'(call  ,self))

(print (call 'a 'b)) 
(print (call + 2 3)) 
