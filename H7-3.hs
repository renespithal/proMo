import qualified Data.Map.Strict as Mp

freq :: Ord a => [a] -> Mp.Map a Int
freq []     = Mp.empty
freq (x:xs) = helper x (freq xs)

helper :: Ord a => a -> Mp.Map a Int -> Mp.Map a Int
helper x firstMap = finalMap
   where
   finalMap = Mp.insertWith (+) x 1 firstMap

--insertWith
--first occurrence it stores 1,
--if there is a previous value x in the old map. 1 + x
