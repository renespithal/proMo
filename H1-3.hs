punkteImKreis :: Double -> [( Double , Double )]
punkteImKreis k = [ (x, y) | x <- [1.. k] , y <- [1.. k], sqrt (x^2 + y^2) < k ]

len :: [(Double,Double)] -> Double
len [] = 0
len (_:xs) = 1 + len xs

anteilImKreis :: Double -> Double
anteilImKreis k = (len $ punkteImKreis k) / k^2
