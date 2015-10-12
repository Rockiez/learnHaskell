
-- 使用模式匹配和递归实现length
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
-- 输入一个list，用_变量将第0位剥离，将后面的部分作为list传入函数，
-- 然后算上去掉的那位”1“，再次将剩余部分递归运算。

--使用模式匹配和递归实现sum
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 只要把 sum' xs 看作一个有返回值的整体，不要去想其中的过程。

-- 使用all@(模式)的形式，all可以引用整体
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ "is" ++ [x]
-- 注意不要在模式匹配中使用＋＋

-- Guards
-- RealFload是Floating和RealFrac的instace（实例类型）
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
      | weight / height ^ 2 <= 18.5 = "You're underweight,you emo ,you!"
      | weight / height ^ 2 <= 25.0 = "You're supposedly normail. Pfff. i bet you;re ugly!"
      | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty"
      | otherwise                   = "You're a whale, congratulations!"

-- 使用Guards重写max函数
max' :: (Ord a) => a -> a -> a
max' a b
    |a > b     = a
    |otherwise = b

-- 使用Guards重写myCompare函数
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  |a > b = GT
  |a == b = EQ
  |otherwise = LT
-- 通过反单引号，我们不仅可以以中缀形式调用函数，也可以在定义函数的时候使用它，
-- 这样更加易读

-- 使用where语句为Guards添加变量
-- where绑定为函数内部建立变量，而不是从参数变量那获取。
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
        |bmi <= skinny = "You're underweight,you emo ,you!"
        |bmi <= normal = "You're supposedly normail. Pfff. i bet you;re ugly!"
        |bmi <= fat    = "You're fat! Lose some weight, fatty"
        |otherwise     = "You're a whale, congratulations!"
        where bmi = weight / height ^ 2
              (skinny, normal, fat) = (18.5, 25.0, 30.0)
-- where绑定同样可以使用匹配。
-- 前面是模式，后面是要进行匹配的内容。


initials' :: String -> String -> String
-- initials' :: (String a) => a -> a -> a
-- initials' firstname lastname = [f] ++ "." ++ [l] ++ "."
--   where (f:_) = firstname
--         (l:_) = lastname
initials' (f:_) (l:_) = [f] ++ "." ++[l] ++ "."

-- where绑定不一定是Guards上，也可以直接绑定在函数中。甚至可以where一层层的绑定。
calcBmis :: (RealFloat a ) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
          where bmi weight height = weight / height ^ 2


-- 使用let和in表达式构建计算柱体面积
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^2
  in  sideArea + 2 * topArea
-- let中绑定的名字只有in中可见，let，in表达式可以随处摆放，因为它是一个返回一个值的表达式。

calcBmis' :: (RealFloat a ) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^2]
-- 集合使用let...in语句，可以省略in部分，此时let定义的名字在‘输出函数’‘限制条件’部分是可见的，
-- 但是在枚举和集合部分是不可见的。如果不省略in，那么只有in部分是可见的。

describeList :: [a] -> String
describeList xs = "The list is" ++ case xs of [] -> "empty"
                                              [x] -> "a singleton list"
                                              xs -> "a longer list"


describeList' :: [a] -> String
describeList' xs = "The list is " ++ let what :: [a] -> String
                                         what [] = "empty"
                                         what [x] = "a singleton list"
                                      in what xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [ a | a <- xs, a <= x]
      biggerSorted  = quicksort [ a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted