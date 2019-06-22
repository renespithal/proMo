import qualified Data.List as Gr
binaryToIntList:: [Bool] -> Int
binaryToIntList xs = sum (map (*2^2)(map fromEnum xs))

upto :: Eq a => (a -> Bool) -> [a] -> [a]
upto f xs = filter f xs

removeAdjacentDuplicates :: Eq a => [a] -> [a]  
removeAdjacentDuplicates = map head . Gr.group
