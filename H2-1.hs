{-# OPTIONS_GHC -Wincomplete-patterns #-}
sort2 :: (Int , Int) -> (Int , Int)
sort2 (x,y) = case x > y of
                True -> (y,x)
                _ -> (x,y)

sort3 :: (Int , Int , Int) -> (Int , Int , Int)
sort3 (x,y,z)  = case x<y && y<z of
                   True -> (x,y,z)
                   _ -> case x<z && y<z of
                     True -> (y,x,z)
                     _ -> case z<y && y<x of
                       True -> (z,y,x)
                       _ -> case y<z of
                         True -> (y,z,x)
                         _ -> case x<z of
                           True -> (x,z,y)
                           _ -> (z,x,y)

sortFirst3 :: [Int] -> [Int]
sortFirst3 [] = []
sortFirst3 [x] = [x]
sortFirst3 [x,y] = case x > y of
                      True -> [y,x]
                      _ -> [x,y]
sortFirst3 (x:y:z:xs) = case x<y && y<z of
                           True -> [x,y,z] ++ xs
                           _ -> case x<z && y<z of
                             True -> [y,x,z] ++ xs
                             _ -> case z<y && y<x of
                               True -> [z,y,x] ++ xs
                               _ -> case y<z of
                                 True -> [y,z,x] ++ xs
                                 _ -> case x<z of
                                   True -> [x,z,y] ++ xs
                                   _ -> [z,x,y] ++ xs
