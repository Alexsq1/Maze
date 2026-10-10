module Main where

import Search
import Maze
import Data.List


main :: IO ()
main = do
    
    let walls1 = (genWalls (1,2) (1,8)) ++ (genWalls (1,1) (4,1)) ++ (genWalls (8,1) (9,1)) ++ (genWalls (4,4) (4,8)) ++ (genWalls (6,2) (6,8)) ++ [(7,9), (5,3), (4,0)]
    let ms1 = newMaze 10 10 walls1
    --let mu1 = newMaze 10 10 ((4,2):walls1)
    let ini = (0,0)
    let fin = (9,9)
    
    allSearchs ms1 ini fin
    --allSearchs mu1 ini fin


    --let walls2 = ([(x,y) | x <- [1,3 .. 18], y <- [1 .. 18]] ++ [(x,0) | x <- [1,5 .. 18]] ++ [(x,19) | x <- [3,7 .. 18]] )
    --let ms2 = newMaze 20 20 walls2
    --let mu2 = newMaze 20 20 ((18,19):walls2)
    --let fin = (19,19)
    
    --allSearchs ms2 ini fin
    --allSearchs mu2 ini fin

    --let walls3 = (genWalls (3,2) (3,11)) ++ (genWalls (3,2) (5,2)) ++ (genWalls (7,0) (9,1)) ++ (genWalls (6,3) (10,3)) ++ (genWalls (11,2) (12,3)) ++ (genWalls (13,2) (13,5)) ++ (genWalls (13,5) (17,5)) ++ (genWalls (17,2) (17,5)) ++ (genWalls (15,0) (15,2)) ++ (genWalls (0,4) (2,4)) ++ (genWalls (16,7) (19,7)) ++ (genWalls (12,7) (15,8)) ++ (genWalls (11,5) (11,7)) ++    (genWalls (5,5) (10,5)) ++ (genWalls (5,5) (5,8)) ++ (genWalls (6,8) (7,8)) ++ (genWalls (4,10) (16,10)) ++ (genWalls (9,7) (9,9)) ++ (genWalls (17,9) (17,12)) ++ (genWalls (4,12) (16,12)) ++ (genWalls (6,14) (19,14)) ++ (genWalls (1,6) (1,18)) ++ (genWalls (1,18) (6,18)) ++ (genWalls (6,16) (6,17)) ++ (genWalls (6,15) (19,15)) ++ (genWalls (8,17) (17,19)) ++ [(3,0), (18,2)]
    --let ms3 = newMaze 20 20 walls3
    --let mu3 = newMaze 20 20 ((16,16):walls3)
    --let ini = (0,0)
    --let fin = (19,19)
    --allSearchs ms3 ini fin
    --allSearchs mu3 ini fin

    --laberinto 3: intratable

--    let walls4 = [(x,y) | x <- [3,7 .. 14], y <- [3,7 .. 14], not (elem y [1, 5, 9])] \\ [(11,14)]
    --let walls4 = ([(x,y) | x <- [3,7 .. 14], y <- ([0 .. 14] \\ [1,5,9])] ++ [(x,y) | x <- ([0 .. 14]), y <- [3,7 .. 14]] ) \\ [(13,3),(11,14),(1,7)]
    --let mu4 = newMaze 15 15 ((9,11):(11,1):walls4)
    --let ms4 = newMaze 15 15 (walls4)
    --let ini = (0,0)
    --let fin = (14,14)
    --allSearchs ms4 ini fin
    --allSearchs mu4 ini fin
    --laberinto 4: intratable salvo para IDA* (1 minuto)

allSearchs :: Maze -> Square -> Square -> IO()
allSearchs m ini fin = do

    putStrLn "Probando para el laberinto: "
    putStrLn $ show m
    putStrLn $ "\nProbando IDA* en laberinto:\n" ++ show (idaStar m (manhattanDistance fin) ini fin)
    putStrLn $ "\nProbando A* en laberinto:\n" ++ show (astar m (manhattanDistance fin) ini fin)
    --putStrLn $ "\nProbando iddfs:\n" ++ show (iddfs m ini fin)
    --putStrLn $ "\nProbando simpleIddfs:\n" ++ show (simpleIddfs m ini fin)
    --putStrLn $ "\nProbando dfs:\n" ++ show (dfs m 1000 ini fin)
    --putStrLn $ "\nProbando bfs:\n" ++ show (bfs m ini fin)
    