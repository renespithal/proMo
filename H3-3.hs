insertBeforeLast :: String -> Char -> String
insertBeforeLast [] _ = []
insertBeforeLast [x,y] a = [x,a,y]
insertBeforeLast (x:xs) a = x : insertBeforeLast xs a
