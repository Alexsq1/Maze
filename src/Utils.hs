module Utils (unique, zipSort) where

-- | Implementation of quickSort algorithm
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ 
			[y | y <- xs, y == x] ++
			quickSort [y | y <- xs, y > x]

-- | Splits a list in two halves
splitHalf :: [a] -> ([a], [a])
splitHalf xs = (ys, zs)
    where
        l = length xs
        ys = [xs !! i | i <- [0 .. (l - 1)], 2*i < l]
        zs = [xs !! i | i <- [0 .. (l - 1)], 2*i >= l]

-- | Implementation of mergeSort algorithm
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = zipSort ysOrd zsOrd
    where 
        (ys, zs) = splitHalf xs
        ysOrd = mergeSort ys
        zsOrd = mergeSort zs

-- | Auxiliar of mergeSort joins two sorted lists into a sorted one
--Recibe 2 listas ordenadas, las entrelaza
zipSort :: Ord a => [a] -> [a] -> [a]
zipSort [] xs = xs
zipSort xs [] = xs
zipSort (x:xs) (y:ys)
    | x < y = x : zipSort xs (y:ys)
    | otherwise = y : zipSort (x:xs) ys


-- | Eliminate repeated elements
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x : (filter (/= x)) (unique xs)

