-- Напишите функцию, зависящую от двух аргументов x и y,
-- удаляющую все вхождения x в список y. Х может быть атомом или списком.

removeElem :: Eq a => a -> [a] -> [a]
removeElem _ [] = []
removeElem y (x:xs)
  | x == y = removeElem y xs
  | otherwise = x : removeElem y xs

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll [] x = x
removeAll (y:ys) x = removeAll ys (removeElem y x)

main :: IO ()
main = do
  let x = [1, 2, 3, 4, 5, 6, 7]
  print $ removeAll [3] x
  print $ removeAll [2, 5] x