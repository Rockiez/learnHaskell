
maximun' :: (Ord a) => [a] -> a
maximun' [] = error "maximun of empty list"
maximun' [x] = x
maximun' (x:xs) = max x (maximun' xs)
-- 一段list中最大的数目

-- 声明类型可以重复声明不同的类型,数字类型中没有比较的函数
replicate' :: (Num i, Ord i) => i -> a -> [a] 
replicate' n x
        | n <= 0  = []
        | otherwise = x: replicate' (n - 1)  x


take' :: (Num i ,Ord i) => i -> [a] -> [a]
take' n _
      |n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
-- guard没有使用otherwise兜底说明如果大于一就会直接进入下个模式。

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x  = True
    | otherwise = a `elem` xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [ a | a <- xs, a <= x]
        biggerSorted  = quicksort [ a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
-- 编辑器有bug，导致看不出是否对齐，导致报错。
