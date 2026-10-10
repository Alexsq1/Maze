module DataStructures where

newtype Fifo a = Fifo [a] deriving (Show, Read, Eq)

newFifo :: (Eq a) => [a] -> Fifo a
newFifo = Fifo

enqueue :: a -> Fifo a -> Fifo a
enqueue e (Fifo xs) = Fifo (xs ++ [e])

enqueueMult :: (Eq a) => [a] -> Fifo a -> Fifo a
enqueueMult xs (Fifo ys) = newFifo (ys ++ xs)

concatFifos :: (Eq a) => Fifo a -> Fifo a -> Fifo a
concatFifos (Fifo xs) (Fifo ys) = newFifo (xs ++ ys)

dequeue :: (Eq a) => Fifo a -> Maybe (a, Fifo a)
dequeue (Fifo xs)
    | null xs = Nothing
    | otherwise = Just (h, newFifo t)
    where
        (h: t) = xs

instance Foldable Fifo where
    foldr f n (Fifo xs) = foldr f n xs

