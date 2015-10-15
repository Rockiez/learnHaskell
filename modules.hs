findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
-- findKey key [] = Nothing
-- findKey key ((k, v):xs) =
-- 	if key  == k then
-- 		Just v
-- 	else
-- 		findKey key xs


-- Maybe类型保证即使是空list也不会让head函数崩溃——他会返回一个Nothing，
-- 如果找到就返回Just something。

findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- 等号左右都有[(k,v)]就可以被抵消。相当于传入一个key参数，
-- 返回一个需要一个参数的函数。
-- 一般来说，使用fold函数比递归更好些。

-- 以上的关联列表在Data.Map中有更好的实现。
-- findKey对应其中的lookup函数。

import qualified Data.Map as Map

-- Prelude作为缺省模块自动加载，其中有一些常用的函数与Map模块重名
-- 使用qualified选项可以保留原有的函数，并将新函数别名为Map

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ "," ++ number2) xs
-- fromListMap不会直接忽略重复键，而是交给一个函数处理
-- 一旦出现重复键，这个函数就会将不同的值组在一起。