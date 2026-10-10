-- | Search algorithms

module Search where

import Maze
import DataStructures
import Utils
import Data.List
import Data.Either

-- | Euclidean distance (squared) between 2 squares
euclidDistance :: Square -> Square -> Float
euclidDistance (x1, y1) (x2, y2) = sqrt (((xf1 - xf2) ^ 2) + ((yf1 - yf2) ^ 2))
    where
        [xf1, xf2, yf1, yf2] = map (fromInteger) [x1, x2, y1, y2]

-- | Manhattan distance between 2 squares
manhattanDistance :: Square -> Square -> Integer
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Generates the adjacent valid squares of a given one
adjacent :: Maze -> Square -> [Square]
adjacent m (x, y) = filter (\t -> (not (isWall m t)) && validSquare m t) cands
    where 
        cands = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

-- | Takes a path, returns all the paths with 1 more square
expandValidPaths :: Maze -> [Square] -> [[Square]]
expandValidPaths m [] = [[]]
expandValidPaths m currPath = xpValids
    where
        lastSquare = last currPath
        adjSquares = adjacent m lastSquare

	-- concatenate all adjacent squares of the last one to the previous path
        expanded = map (\new -> currPath ++ [new]) adjSquares    

	-- only select those that increased the length after deleting duplicates (deleting cycles)
        xpValids = filter (\path -> length path > length currPath) (map unique expanded)          
          

{-
walls1 = (genWalls (1,2) (1,8)) ++ (genWalls (1,1) (4,1)) ++ (genWalls (8,1) (9,1)) ++ (genWalls (4,4) (4,8)) ++ (genWalls (6,2) (6,8)) ++ [(7,9), (5,3), (4,0)]
-}

          
-- | Checks that all the squares are in the range of the maze and are not walls
checkSquares :: Maze -> [Square] -> Bool
checkSquares m xs = and (map (validSquare m) xs) && and (map (\sq -> not (isWall m sq)) xs)

-- | Depht first search with limit
dfs :: Maze 	    -- ^ Maze
	-> Integer  -- ^ Limit
	-> Square   -- ^ Source
	-> Square   -- ^ Destiny
	-> [Square]
dfs m dMax ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = dfs_sgle m 0 dMax [] ini fin

    where

        --dfs_sgle generates branches and recomposes the path
        dfs_sgle :: Maze     -- ^ maze
		-> Integer   -- ^ current depth
		-> Integer   -- ^ maximum depth
		-> [Square]  -- ^ visited squares
		-> Square    -- ^ source
		-> Square    -- ^ destiny
		-> [Square]
        dfs_sgle m dCurr dMax visited ini fin
            | dCurr > dMax = []                         -- maximum depth reached
            | ini `elem` visited = []                   -- initial square is visited
            | ini == fin = [ini]                        -- path found, return the node
            | search_subtree_sgle == [] = []            -- branch did not find a solution, return empty
            | otherwise = ini : search_subtree_sgle     -- branch found a solution, recompose adding the initial node

            where 
                xs = (adjacent m ini) \\ visited
                search_subtree_sgle = dfs_multiple m (dCurr + 1) dMax xs (ini : visited) fin

        --dfs_multiple joins several branches by selecting the one that reached the solution (if existes)

        dfs_multiple :: Maze        -- ^ Maze
			-> Integer  -- ^ Current depth
			-> Integer  -- ^ Maximum depth
			-> [Square] -- ^ Paths to be explored
			-> [Square] -- ^ Visited squares
			-> Square   -- ^ Destiny
			-> [Square] -- ^ 
        dfs_multiple _ _ _ [] _ _ = []                                                      -- all branches traversed, no solution
        dfs_multiple m dCurr dMax (x:xs) visited fin
            | search_subtree_multiple == [] = dfs_multiple m dCurr dMax xs visited fin      -- branch did not find a solution, keep searching
            | otherwise = search_subtree_multiple                                           -- branch found a solution, return the branch
            where 
                search_subtree_multiple = dfs_sgle m dCurr dMax visited x fin

-- | Breadth first search
bfs :: Maze -> Square -> Square -> [Square]
bfs m ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = bfs_aux m (newFifo [[ini]]) fin
    where
        bfs_aux :: Maze -> Fifo [Square] -> Square -> [Square]
        bfs_aux m fifo fin
            | null fifo = []                                            -- Empty fifo: nothing more to explore, did not find a solution
            | lastSquare == fin = currPath                          	-- Found: return path
            | otherwise = bfs_aux m nextFifo fin     			-- Not found: expand initial and keep searching
                where 
                    Just (currPath, restFifo) = dequeue fifo
                    lastSquare = last currPath
                    newPaths = expandValidPaths m currPath
                    nextFifo = enqueueMult newPaths restFifo



-- | Iterative deepening depht-first search: do dfs with a bound, augment the maximum depth until a solution is not found or the whole maze was explored
iddfs :: Maze -> Square -> Square -> [Square]
iddfs m ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = iddfsIterate m 1 ini fin
        where
            iddfsIterate :: Maze -> Integer -> Square -> Square -> [Square]
            iddfsIterate m n ini fin
                | (not . null) path = path				-- solution found
                | (null path) && pruned = iddfsIterate m (n+1) ini fin	-- solution not found but squares are not traversed
                | otherwise = []					-- all the grid is traversed
                    where
                        (pruned, path) = dfsSgleCustom m 0 n [] ini fin

		-- dfs, the custom feature is returning if the maximum depth was reached (to avoid infinite loops)
            dfsSgleCustom :: Maze -> Integer -> Integer -> [Square] -> Square -> Square -> (Bool, [Square])
            dfsSgleCustom m dCurr dMax visited ini fin
                | dCurr > dMax = (True, [])                          -- maximum depth
                | ini `elem` visited = (False, [])                   -- square visited
                | ini == fin = (False, [ini])                        -- solution found
                | path == [] = (pruned, path)                        -- solution not found in this branch
                | otherwise = (pruned, ini : path)                   -- recompose search

                where 
                    xs = (adjacent m ini) \\ visited
                    search_subtree_sgle = dfsMultipleCustom m (dCurr + 1) dMax xs (ini : visited) fin
                    (pruned, path) = search_subtree_sgle


            dfsMultipleCustom :: Maze -> Integer -> Integer -> [Square] -> [Square] -> Square -> (Bool, [Square])                                                       
            dfsMultipleCustom m dCurr dMax toCheck visited fin
                | null usefulPaths = (recomposePruneds, [])
                | otherwise = (recomposePruneds, (head usefulPaths))

                where
                    search = map (\sq -> dfsSgleCustom m dCurr dMax visited sq fin) toCheck   -- each node is mapped with a search
                    (pruneds, paths) = unzip search         
                    usefulPaths = filter (not . null) paths
                    recomposePruneds = or pruneds
		    -- the boolean pruneds are joined with an or

-- Either could be used to avoid a boolean?

-- | Similar iddfs, but the maximum depth is the number of squares (simplifies the code a lot)
simpleIddfs :: Maze -> Square -> Square -> [Square]
simpleIddfs m ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = iddfsIterate m 1 max ini fin

    where
        max = numSquares m

        iddfsIterate :: Maze -> Integer -> Integer -> Square -> Square -> [Square]
        iddfsIterate m n max ini fin
            | n > max = []
            | subsearch == [] = iddfsIterate m (n+1) max ini fin
            | otherwise = subsearch
                where
                    subsearch = dfs m n ini fin

-- | Path datatype [Squares] (Current length) (Expected length with heuristic)
data Path = Path [Square] Integer Integer deriving (Eq, Show)

instance Ord Path where
    compare (Path xs1 real1 pred1) (Path xs2 real2 pred2) = compare pred1 pred2

-- | General A* search
astar :: Maze			-- ^ Maze
	-> (Square -> Integer)  -- ^ Heuristic
	-> Square 		-- ^ Source
	-> Square 		-- ^ Destiny
	-> [Square]
astar m heuristic ini fin 
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = astarAux m heuristic [Path [ini] 0 (heuristic ini)] fin
    where
    	-- Receives a list with the paths to be explored
        astarAux :: Maze -> (Square -> Integer) -> [Path] -> Square -> [Square]
        astarAux _ _ [] _ = []						-- nothing to explore
        astarAux m heuristic ((Path route real expected) : rest) fin
            | lastSquare == fin = route                                 -- solution found
            | nextPathSituation == [] = astarAux m heuristic rest fin	-- nothing to expand in the current square, keep searching
            | otherwise = astarAux m heuristic nextPathSituation fin 	-- keep searching

            where
                lastSquare = last route

                adjs = (adjacent m lastSquare)
                sqsExpanded = map (\new -> route ++ [new]) adjs    -- expand current path with adacents

                adjsValid = filter (\path -> length path > length route) (map unique sqsExpanded) -- Avoid cycles or repetitions

		-- Generate with format Path [Squares] Integer, calculating expected path length
                genPath = map (\sqs -> Path sqs (real + 1) (real + (heuristic (last sqs)))) adjsValid      

                nextPathSituation = zipSort genPath rest

-- To be improved: priority queue of squares

-- | IDA* algorithm: IDDFS with heuristics
idaStar :: Maze 		-- ^ Maze
	-> (Square -> Integer)  -- ^ Heuristic
	-> Square               -- ^ Source
	-> Square               -- ^ Destiny
	-> [Square] 
idaStar m heuristic ini fin 
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = idaStarAux m heuristic (heuristic ini) (numSquares m) ini fin
    where
        idaStarAux :: Maze -> (Square -> Integer) -> Integer -> Integer -> Square -> Square -> [Square]
        idaStarAux m heuristic dCurr ultimateD ini fin 
            | dCurr > ultimateD = []                                                            -- All the squares are explored, no solution
            | isLeft thisSearch && pruned = idaStarAux m heuristic dLongest ultimateD ini fin   -- Solution not found with pruning: augment max depth
            | isRight thisSearch = fromRight [] thisSearch                                      -- Solution found
            | otherwise = []                                                                    -- All the squares are explored, no solution
            where
                thisSearch = dfs_sgle m heuristic 0 dCurr [] ini fin                            -- DFS search
                (pruned, dLongest) = fromLeft (False, 0) thisSearch                             -- Restore values

	-- Returns Right [Sq] if a solution is found and Left (pruned, max estimated length) when no solution
        dfs_sgle :: 
            Maze -> (Square -> Integer) -> Integer -> Integer -> [Square] -> Square -> Square -> Either (Bool, Integer) [Square]
        dfs_sgle m heuristic dCurr dMax visited ini fin
            -- | dCurr > dMax = Left (True, (dCurr + heuristic ini))                        -- max depth reached: prune
            | thisSqEstimatedPath > dMax = Left (True, thisSqEstimatedPath)                 -- longer estimated path
            | ini == fin = Right [ini]                                                      -- solution found
            | ini `elem` visited = Left (False, 0)                                          -- visited square
            | null adj = Left (False, (dCurr + heuristic ini))                              -- path without adjacents
            | isLeft branch = branch                                                        -- solution not found: augment max depth
            | otherwise = Right (ini : (fromRight [] branch))                               -- solution found: recompose

            where 
                thisSqEstimatedPath = dCurr + (heuristic ini)                               -- path estimation 
                adj = (adjacent m ini) \\ visited                                           -- Non-visited adjacents
                branch = dfs_multiple m heuristic (dCurr + 1) dMax adj (ini : visited) fin  -- child search

        dfs_multiple :: 
            Maze -> (Square -> Integer) -> Integer -> Integer -> [Square] -> [Square] -> Square -> Either (Bool, Integer) [Square]

        dfs_multiple m heuristic dCurr _ [] _ _ = Left (False, dCurr)                 -- all the branches are searched, no solutin
        dfs_multiple m heuristic dCurr dMax (x:xs) visited fin                          -- search
            | isRight thisBranch = thisBranch                                           -- Solution found in this branch: recompose
            | isRight (otherBranches) = otherBranches                                   -- Solution found in another branch
            | otherwise = Left ((pruned1 || pruned2), (max num1 num2))                  -- Solution not found: unify branches

            where 
                thisBranch = dfs_sgle m heuristic dCurr dMax visited x fin              -- Keep searching
                otherBranches = dfs_multiple m heuristic dCurr dMax xs visited fin      -- Search recursively other branches
                (pruned1, num1) = fromLeft (False, 0) thisBranch
                (pruned2, num2) = fromLeft (False, 0) otherBranches

		-- would be simpler if we knew a solution exists. Idea: check that the final square has adj squares at the beginning
