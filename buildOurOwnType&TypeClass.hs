import qualified Data.Map as Map
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- 建立了一个Shape的type，它声明两个Value Constructor(函数).
-- 它们最后返回Shape类型的值。

surface :: Shape -> Float
surface (Circle  _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 -y1)
-- 这里模式匹配针对的是ValueConstructor。

data Person = Person{
    firstName :: String,
    lastName :: String,
    age :: Int
} deriving(Show)
-- 单个值构造子与type同名是非常常见的事情。
-- 这样自动生成“获取其中某一项”的函数。而且在生成intance的时候也不用在意顺序。
-- sam = Person{firstName="Sam", age = 12,lastName = "Simo" }

data Vector a =  Vector a a a deriving(Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
-- 型别声明只能写型别，型别是Vector a，那就只能写Vector t了。
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

data Day = Monday 
         | Tuesday 
         | Wednesday 
         | Thurday 
         | Friday 
         | Saturday 
         | Sunday 
         deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- 如果等式的右侧需要另起一行，可以在行前添加一些空格。
-- Ord的大小判断是根据值构造子的顺序——左小右大。
-- Eq和Ord的先判断是否是同个值构造子，如果相同检查所有数据是否一致
-- 或者判断其参数的Ord。


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
-- type的作用是给现有的type设置别名，甚至是给type安排好接受参数的type。
-- 这里的Map.Map表示调用Map模块中的type，Map是二参型别，
-- 一个是Int一个是Tuple，同样需要将Tuple其中储存的数据的类型安排好。

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case Map.lookup lockerNumber map of
        Nothing -> Left $ show lockerNumber
        Just (state, code) -> if state /= Taken
                             then Right code
                             else Left $ show lockerNumber

-- Either型别冲Left和Right中取其一，并一起返回一个任意型别的数据。

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
         | x == a = Node x left right
         | x < a = Node a (treeInsert x left) right
         | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = false
treeElem x (Node a left right)
       | x == a = True
       | x < a = treeElem x left
       | x > a = treeElem x right








