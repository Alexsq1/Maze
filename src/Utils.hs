-- | Auxiliar functions

module Utils (unique, zipSort) where

-- | Auxiliar of mergeSort joins two sorted lists into a sorted one
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

