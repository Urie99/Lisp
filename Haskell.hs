-- Списки: [4, 6, 9, 16, 24], коды: [3], деревья: [1]
-- Задача №6
-- Реализовать функцию обратную к функции из задачи 5(Определите функцию, упаковывающую последовательные дубликаты списка 
-- в подсписки вида (M N), где N - элемент списка, M - количество повторений. 
-- Например, ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] должен быть переведен в 
-- [(4, 'a'), (1, 'b'), (2, 'c'),(2, 'a'), (1, 'd'), (4, 'e')].
--encode list = concat (map (\x -> replicate (length x) (head x)) list)
--encode [(1, 'a'), (2, 'b'), (3, 'c'),(4, 'd'), (5, 'e')]
-- **************************************************************************************************************************
-- Задача №16
-- Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список
-- множеством, т.е. входит ли каждый элемент в список лишь один раз.

mnozhestvo p = length (filter (>1) (map(\x -> (length . filter (==x))p) p)) == 0
mnozhestvo [5,4,3]
mnozhestvo [1,1,2]
mnozhestvo [7,8,9]
