import qualified Data.List as Gr
binaryToIntList:: [Bool] -> Int
binaryToIntList [] = error("List cant be empty")
binaryToIntList xs = sum(zipWith (\i x -> x * 2^i) [length xs -1, length xs -2..0] (map fromEnum xs))

upto :: Eq a => (a -> Bool) -> [a] -> [a]
upto f xs = filter (not . f) xs

removeAdjacentDuplicates :: Eq a => [a] -> [a]  
removeAdjacentDuplicates = map head . Gr.group
