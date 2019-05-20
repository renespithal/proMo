{-# OPTIONS_GHC -Wincomplete-patterns #-}
sort2 :: (Int , Int) -> (Int , Int)
sort2 (x,y) = case x > y of
                True -> (y,x)
                _ -> (x,y)

sort3 :: (Int,Int,Int) -> (Int,Int,Int)
sort3 (x,y,z)
  | x<y && y<z = (x,y,z)
  | x<z && y<z = (y,x,z)
  | z<y && y<x = (z,y,x)
  | y<z        = (y,z,x)
  | x<z        = (x,z,y)
  | otherwise  = (z,x,y)

sortFirst3 :: [Int] -> [Int]
sortFirst3 [] = []
sortFirst3 [x] = [x]
sortFirst3 [x,y]
  | x > y     = [y,x]
  | otherwise = [x,y]
sortFirst3 (x:y:z:xs)
  | x<y && y<z = [x,y,z] ++ xs
  | x<z && y<z = [y,x,z] ++ xs
  | z<y && y<x = [z,y,x] ++ xs
  | y<z        = [y,z,x] ++ xs
  | x<z        = [x,z,y] ++ xs
  | otherwise  = [z,x,y] ++ xs
