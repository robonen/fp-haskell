-- Определите функцию (f a n), которая от двух числовых
-- аргументов строит следующий список:
-- [a, a*(a+1),a*(a+1)*(a+2),…,a*(a+1)*(a+2)*…*(a+n)].

f :: Int -> Int -> [Int]
f a n = scanl (*) a $ take n [a+1..]

main :: IO ()
main = do
  let a = 2
  let n = 5
  print $ f a n
