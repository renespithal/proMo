punkteImKreis :: Double -> [( Double , Double )]
punkteImKreis k = [(a,b) | a <- [1..(k-1)], b <- [1..(k-1)]]

len :: [(Double,Double)] -> Double
len [] = 0
len (_:xs) = 1 + len xs

anteilImKreis :: Double -> Double
anteilImKreis k = (len $ punkteImKreis k) / k^2
