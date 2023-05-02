-- Напишите функцию (f s n), которая из двухуровневого
-- списка чисел s создает новый одноуровневый список квадратов
-- чисел, исключив все элементы исходного списка s, которые
-- меньше заданного числа n.

f :: [[Int]] -> Int -> [Int]
f s n = [x*x | xs <- s, x <- xs, x >= n]

main :: IO ()
main = do
  let s = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  print $ f s 5