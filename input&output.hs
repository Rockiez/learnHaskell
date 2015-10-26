main = do
    line = getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- do block返回最后一个IO cations。if语句必须返回一个结果。
-- return 将值包装成IO cations，而<-将值从IO cations中提出出来。
-- do block中的最后一个cation不能绑定任何名字。
