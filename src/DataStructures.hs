-- | Implementation of a Fifo

module DataStructures (Fifo(..), newFifo, enqueueMult, dequeue) where

-- | Fifo list
newtype Fifo a = Fifo [a] deriving (Show, Read, Eq)

-- | Fifo creation
newFifo :: (Eq a) => [a] -> Fifo a
newFifo = Fifo

-- | Inserts an element at the end of the fifo
enqueue :: a -> Fifo a -> Fifo a
enqueue e (Fifo xs) = Fifo (xs ++ [e])

-- | Inserts several elements
enqueueMult :: (Eq a) => [a] -> Fifo a -> Fifo a
enqueueMult xs (Fifo ys) = newFifo (ys ++ xs)

-- | Concatenates two fifos
concatFifos :: (Eq a) => Fifo a -> Fifo a -> Fifo a
concatFifos (Fifo xs) (Fifo ys) = newFifo (xs ++ ys)

-- | Takes the first element, if the fifo is not empty
dequeue :: (Eq a) => Fifo a -> Maybe (a, Fifo a)
dequeue (Fifo xs)
    | null xs = Nothing
    | otherwise = Just (h, newFifo t)
    where
        (h: t) = xs

instance Foldable Fifo where
    foldr f n (Fifo xs) = foldr f n xs

