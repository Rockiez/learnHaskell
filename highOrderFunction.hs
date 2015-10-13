zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- 第一个参数是一个函数，第二，三个参数list，并返回一个list。
-- 这个参数函数的第一个参数和整个函数的第二个参数相同，第二个和第三个相同，
-- f x y 返回 c，通过递归变成［c］

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain(n `div` 2)
    | odd n = n:chain(n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + c) 0 xs
sum' = foldl (+) 0
-- used curring

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys



map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x:acc) [] xs
-- 在生成一个list的时候使用：效率的foldr要比使用＋＋的foldl高很多。

sum'' :: (Num a) => [a] -> a
sum'' = foldl1 (+)
-- foldl1 & foldr1 不需要初始值，他们将list的第一个数字作为初始值。
-- 如果初始值为0，就可以使用这个。

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)
-- 返回一个需要一个list的函数。

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
-- 不定义空list，构不成list。

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)
-- 从最后第二位开始取，一直取到第一个。

last' :: [a] -> a
last' = foldr1 (\_ x -> x)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sqrtSums :: (Num a) => a
-- sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

sqrtSums = length' (takeWhile (<1000000) $ scanl1 (+) $ map sqrt [1..]) + 1

