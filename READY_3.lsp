;Макросы
;Задача 1
;Определите макрос, который возвращает свой вызов.
(defmacro call (&rest self)
   `'(call  ,self))

(print (call 'a 'b)) 
(print (call + 2 3)) 
;-------------------------------------------------------------------------------------

;Задача 2
;Определите макрос (POP стек), который читает из стека верхний элемент и
;меняет значение переменной стека.
(defmacro POP-stack (stack)
  `(prog1
     (setq first (car ,stack))
     (setq ,stack (cdr ,stack))))

(setq num-stack `(3 2 1))

(print (POP-stack num-stack))
(print (POP-stack num-stack))
(print (POP-stack num-stack))

