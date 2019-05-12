-- ich gehe davon aus das u != U
count :: Char -> String -> Int 
count n [] = 0
count n (x:xs) = case n==x of
                    True  -> 1 + count n xs
                    False -> 0 + count n xs

-- wusste nicht wie ich ein Set in Haskell mache deswegen einfach über A-z iteriert
freq :: String -> [(Char, Int)]
freq xs = [(x,n) | x <- ['A'..'z'], let n = (length.filter (==x)) xs, n > 0]
