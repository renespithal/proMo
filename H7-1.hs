binaryToIntList:: [Bool] -> Int
binaryToIntList xs = sum (map (*2^2)(map fromEnum xs))

upto :: Eq a => (a -> Bool) -> [a] -> [a]
upto f xs = filter f xs

removeAdjacentDuplicates :: (Ord a) => [a] -> [a]
removeAdjacentDuplicates = map head . group . sort
-- [3,4,2,1,2,3,4,1].
-- sort -> [1,1,2,2,3,3,4,4]
-- group -> [[1, 1], [2, 2], [3,3], [4,4]];
-- head -> [1, 2, 3, 4]
