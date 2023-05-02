-- На вход функции подается два числовых списка [a1, a2, …,an]
-- и [b1, b2, …, bn]. Функция должна формировать новый
-- список [min(a1,b1), min(a2,b2), …, min(an,bn)].

mmin :: Ord a => a -> a -> a
mmin x y = if x < y then x else y

minList :: [Int] -> [Int] -> [Int]
minList [] _ = []
minList _ [] = []
minList (x:xs) (y:ys) = mmin x y : minList xs ys

main :: IO ()
main = do
  let a = [0, 5, 10, 3]
  let b = [5, 1, 3]
  print $ minList a b