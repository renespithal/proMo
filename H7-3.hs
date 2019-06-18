import qualified Data.Map.Strict as Mp

helper :: Int -> Mp.Map Int Int -> Mp.Map Int Int
helper x firstMap = finalMap
   where
   finalMap = Mp.insertWith (+) x 1 firstMap

--insertWith
--first occurrence it stores 1,
--if there is a previous value x in the old map. 1 + x

freq :: [Int] -> Mp.Map Int Int
freq []     = Mp.empty
freq (x:xs) = helper x (freq xs)
