zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- 第一个参数是一个函数，第二，三个参数list，并返回一个list。
-- 这个参数函数的第一个参数和整个函数的第二个参数相同，第二个和第三个相同，
-- f x y 返回 c，通过递归变成［c］
