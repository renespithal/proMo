count :: Char -> String -> Int 
count n [] = 0
count n (x:xs) = case n==x of
                    True  -> 1 + count n xs
                    False -> 0 + count n xs

--freq :: String -> [(Char, Int)]
