-- | Maze representation
module Maze(Square, Maze, newMaze, isWall, genWalls, validSquare, numSquares) where

type Square = (Integer, Integer)

-- | Maze representation with empty squares and walls
data Maze = Maze {grid :: [Square]
		, walls :: [Square]
		, rows :: Integer
		, columns :: Integer}

-- | Creates a maze
newMaze :: Integer  -- ^ Number of rows
	-> Integer  -- ^ Number of columns
	-> [Square] -- ^ List of walls squares 
	-> Maze
newMaze newRows newColumns newWalls = Maze {grid = thisGrid, walls = newWalls, rows = newRows, columns = newColumns}
    where
        thisGrid = [(x, y) | x <- [0 .. newRows - 1], y <- [0 .. newColumns - 1]]

instance Show Maze where
    show maze = concat(map (displayNthRow) [0 .. (rows maze) - 1])
        where
            displayNthRow :: Integer -> String
            displayNthRow index = concat (map (displaySq maze) (nthRow maze index)) ++ "\n"

nthRow :: Maze -> Integer -> [Square]
nthRow m index = [(index, x) | x <- [0 .. (columns m) - 1]]

--nthColumn :: Maze -> Integer -> [Square]
--nthColumn m index = [(x, index) | x <- [0 .. (rows m) - 1]]

displaySq :: Maze -> Square -> String
displaySq m sq = if (isWall m sq) then "X " else ". "

-- | Checks if a specific square is a wall in a maze
isWall :: Maze -> Square -> Bool
isWall maze sq = elem sq (walls maze)

-- | Generates a wall between 2 squares
genWalls :: (Square) -> (Square) -> [(Square)]
genWalls (x1, y1) (x2, y2) = [(x,y) | x <- [x1 .. x2] , y <- [y1 .. y2]]

-- | Checks if a square is a inside the limits of a maze
validSquare :: Maze -> Square -> Bool
validSquare maze (x, y) = x < (rows maze) && x >= 0 &&
                            y < (columns maze) && y >= 0

-- | Calculates the number of free squares of a maze
numSquares :: Maze -> Integer
numSquares m = (rows m) * (columns m) - ((toInteger . length . walls) m)
