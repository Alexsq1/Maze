module Search where


import Maze
import DataStructures
import Utils
import Data.List
import Data.Either

 
euclidDistance :: Square -> Square -> Float
euclidDistance (x1, y1) (x2, y2) = sqrt (((xf1 - xf2) ^ 2) + ((yf1 - yf2) ^ 2))
    where
        [xf1, xf2, yf1, yf2] = map (fromInteger) [x1, x2, y1, y2]

manhattanDistance :: Square -> Square -> Integer
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


adjacent :: Maze -> Square -> [Square]
adjacent m (x, y) = filter (\t -> (not (isWall m t)) && validSquare m t) cands
    where 
        cands = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

expandValidPaths :: Maze -> [Square] -> [[Square]]
expandValidPaths m [] = [[]]
expandValidPaths m currPath = xpValids
    where
        lastSquare = last currPath
        adjSquares = adjacent m lastSquare
        expanded = map (\new -> currPath ++ [new]) adjSquares    --generamos adyacentes, añadiendo el path al inicio. 
        xpValids = filter (\path -> length path > length currPath) (map unique expanded)          
                                                                --de los caminos generados, eliminamos duplicados/ciclos 
                                                                --y los que no añaden no añaden nodos nuevos los borramos
          
{-
Programar:
Búsqueda en anchura         Hecho
Búsqueda en profundidad     Hecho
IDDFS                       Hecho
A*                          Hecho
IDA*

-}

{-
FALTARÍA

A*
Contar heurísticas. Iterar sobre [ [(Square, Integer)] ] (path y distancia desde origen)
Al generar (expand, expandValid etc), calcular dist acumulada + heurística, y ordenar la lista según eso

IDA*
Va a haber que modificar el dfs, pero búsqueda es igual
Si dist acumulada + heur > dMax, podar (guardar el mínimo de los que se podan)
dMax inicial: heurística de inicio
Siguiente iteración: valor mínimo de los que se podaron (guardarlos)
Podría estar chulo explorar las ramas del dfs según heurística

-}


{-
walls1 = (genWalls (1,2) (1,8)) ++ (genWalls (1,1) (4,1)) ++ (genWalls (8,1) (9,1)) ++ (genWalls (4,4) (4,8)) ++ (genWalls (6,2) (6,8)) ++ [(7,9), (5,3), (4,0)]
    
-}

          

checkSquares :: Maze -> [Square] -> Bool
checkSquares m xs = and (map (validSquare m) xs) && and (map (\sq -> not (isWall m sq)) xs)

dfs :: Maze -> Integer -> Square -> Square -> [Square]
dfs m dMax ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = dfs_sgle m 0 dMax [] ini fin

    where
        --dfs_sgle genera ramas y recompone el camino.
        dfs_sgle :: Maze -> Integer -> Integer -> [Square] -> Square -> Square -> [Square]
        dfs_sgle m dCurr dMax visited ini fin
            | dCurr > dMax = []                         --profundidad máxima
            | ini `elem` visited = []                   --ini visitado
            | ini == fin = [ini]                        --camino encontrado: pasar a llamante
            | search_subtree_sgle == [] = []            --rama no lleva a solución
            | otherwise = ini : search_subtree_sgle     --recomponer camino de llamada correcta

            where 
                xs = (adjacent m ini) \\ visited
                search_subtree_sgle = dfs_multiple m (dCurr + 1) dMax xs (ini : visited) fin

        --dfs_multiple va distinguiendo qué ramas llevan --camino y cuáles no, itera sobre las ramas
        dfs_multiple :: Maze -> Integer -> Integer -> [Square] -> [Square] -> Square -> [Square]
        dfs_multiple m _ _ [] _ fin = []                                                    --todas las ramas iteradas
        dfs_multiple m dCurr dMax (x:xs) visited fin
            | search_subtree_multiple == [] = dfs_multiple m dCurr dMax xs visited fin      --fue rama incorrecta, iterar por siguiente rama
            | otherwise = search_subtree_multiple                                           --rama correcta, pasar a llamante para reconstruir camino

            where 
                search_subtree_multiple = dfs_sgle m dCurr dMax visited x fin




--bfs_aux hace la iteración completa
--Itera sobre una fifo de paths

bfs :: Maze -> Square -> Square -> [Square]
bfs m ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = bfs_aux m (newFifo [[ini]]) fin
    where
        bfs_aux :: Maze -> Fifo [Square] -> Square -> [Square]
        bfs_aux m fifo fin
            | null fifo = []                                            --Fifo vacía: devolver nulo
            | lastSquare == fin = currPath                          --encontrado: devolver path
            | otherwise = bfs_aux m nextFifo fin     --no encontrado: expandir inicial y seguir buscando
                where 
                    Just (currPath, restFifo) = dequeue fifo
                    lastSquare = last currPath
                    newPaths = expandValidPaths m currPath
                    nextFifo = enqueueMult newPaths restFifo


--Algoritmo de profundidad iterativa:
--Usa un dfs modificado, va aumentando profunidad máxima iterativamente
--La modificación consiste en devolver el path y si llegó a hacer alguna poda
--Si no lo revisara, cuando no hay camino seguiría aumentando la prof máxima y bucle infinito
--Puede haber mejores formas, pero es la que se me ocurre
--Si se demuesta que hay camino, se puede usar dfs directamente y no revisar casos


iddfs :: Maze -> Square -> Square -> [Square]
iddfs m ini fin
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = iddfsIterate m 1 ini fin
        where
            iddfsIterate :: Maze -> Integer -> Square -> Square -> [Square]
            iddfsIterate m n ini fin
                | (not . null) path = path
                | (null path) && pruned = iddfsIterate m (n+1) ini fin
                | otherwise = []
                    where
                        (pruned, path) = dfsSgleCustom m 0 n [] ini fin

            dfsSgleCustom :: Maze -> Integer -> Integer -> [Square] -> Square -> Square -> (Bool, [Square])
            dfsSgleCustom m dCurr dMax visited ini fin
                | dCurr > dMax = (True, [])                          --profundidad máxima
                | ini `elem` visited = (False, [])                   --ini visitado
                | ini == fin = (False, [ini])                        --camino encontrado: pasar a llamante
                | path == [] = (pruned, path)                        --rama no lleva a solución
                | otherwise = (pruned, ini : path)                   --recomponer camino de llamada correcta

                where 
                    xs = (adjacent m ini) \\ visited
                    search_subtree_sgle = dfsMultipleCustom m (dCurr + 1) dMax xs (ini : visited) fin
                    (pruned, path) = search_subtree_sgle


            dfsMultipleCustom :: Maze -> Integer -> Integer -> [Square] -> [Square] -> Square -> (Bool, [Square])                                                       
            dfsMultipleCustom m dCurr dMax toCheck visited fin
                | null usefulPaths = (recomposePruneds, [])
                | otherwise = (recomposePruneds, (head usefulPaths))

                where
                    search = map (\sq -> dfsSgleCustom m dCurr dMax visited sq fin) toCheck   --a cada nodo lo mapeamos a una búsqueda
                    (pruneds, paths) = unzip search         
                    usefulPaths = filter (not . null) paths
                    recomposePruneds = or pruneds
                    --recuperamos los campos indivs (unzip) y recomponemos solución para pasarla al llamante

--Podría ser más sencillo: Devolver Either 
--Right [Square] -> Camino 
--Left [] -> Alguna poda
--Left [] -> Todo recorrido


--Otra forma más sencilla: fijar límite máximo de iddfs a máx de cuadrados, en este caso, filas * columnas - walls


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


data Path = Path [Square] Integer Integer deriving (Eq, Show)

instance Ord Path where
    compare (Path xs1 real1 pred1) (Path xs2 real2 pred2) = compare pred1 pred2

astar :: Maze -> (Square -> Integer) -> Square -> Square -> [Square]
astar m heuristic ini fin 
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = astarAux m heuristic [Path [ini] 0 (heuristic ini)] fin
    -- En llamante: (manhattanDistance fin )
    where

        astarAux :: Maze -> (Square -> Integer) -> [Path] -> Square -> [Square]
        astarAux _ _ [] _ = []
        astarAux m heuristic ((Path route real expected) : rest) fin
            | lastSquare == fin = route                                 --camino encontrado
            | nextPathSituation == [] = astarAux m heuristic rest fin
            | otherwise = astarAux m heuristic nextPathSituation fin 

            where
                lastSquare = last route

                adjs = (adjacent m lastSquare)
                sqsExpanded = map (\new -> route ++ [new]) adjs    --generamos path actual + adyacentes

                adjsValid = filter (\path -> length path > length route) (map unique sqsExpanded) --Solo considerar [Sq] con camino único (no repetidos ni bucles)

                genPath = map (\sqs -> Path sqs (real + 1) (real + (heuristic (last sqs)))) adjsValid      --Generar con formato Path [Sq] Integer, calculando long esperada
                nextPathSituation = zipSort genPath rest

--Mejorar: nodos en priority queue


idaStar :: Maze -> (Square -> Integer) -> Square -> Square -> [Square]
idaStar m heuristic ini fin 
    | not (checkSquares m [ini, fin]) = error "Not valid squares"
    | otherwise = idaStarAux m heuristic (heuristic ini) (numSquares m) ini fin

        {-idaStarAux devuelve:
        Right [Square] -> Hubo camino (contando vacío)
        Left Integer -> Poda, da mayor camino estimado
        -}

    where
        idaStarAux :: Maze -> (Square -> Integer) -> Integer -> Integer -> Square -> Square -> [Square]
        idaStarAux m heuristic dCurr ultimateD ini fin 
            | dCurr > ultimateD = []                                                            --Llegado a estimación de prof máx: no hay solución
            | isLeft thisSearch && pruned = idaStarAux m heuristic dLongest ultimateD ini fin   --Camino no encontrado con podas: seguir profundizando
            | isRight thisSearch = fromRight [] thisSearch                                      --Camino encontrado, devolver
            | otherwise = []                                                                    --Camino no encontrado, no hay podas: no hay camino
            where
                thisSearch = dfs_sgle m heuristic 0 dCurr [] ini fin                            --Búsqueda dfs
                (pruned, dLongest) = fromLeft (False, 0) thisSearch                             --Recuperar valores

        dfs_sgle :: 
            Maze -> (Square -> Integer) -> Integer -> Integer -> [Square] -> Square -> Square -> Either (Bool, Integer) [Square]
        dfs_sgle m heuristic dCurr dMax visited ini fin
            -- | dCurr > dMax = Left (True, (dCurr + heuristic ini))                           --profundidad máxima: poda
            | thisSqEstimatedPath > dMax = Left (True, thisSqEstimatedPath)                 --ruta estimada mayor que cota: devolver estim. de camino
            | ini == fin = Right [ini]                                                      --camino encontrado: pasar a llamante
            | ini `elem` visited = Left (False, 0)                                          --ini visitado
            | null adj = Left (False, (dCurr + heuristic ini))                              --Camino sin solución (callejón sin salida)
            | isLeft branch = branch                                                        --no se ha encontrado camino, subir cota mín que supere cota
            | otherwise = Right (ini : (fromRight [] branch))                               --camino encontrado: recomponer camino de llamada correcta

            where 
                thisSqEstimatedPath = dCurr + (heuristic ini)                               --estimación de ruta que pasa por nodo actual
                adj = (adjacent m ini) \\ visited                                           --adyacentes no visitados
                branch = dfs_multiple m heuristic (dCurr + 1) dMax adj (ini : visited) fin  --Búsqueda en hijos

        dfs_multiple :: 
            Maze -> (Square -> Integer) -> Integer -> Integer -> [Square] -> [Square] -> Square -> Either (Bool, Integer) [Square]

        dfs_multiple m heuristic dCurr _ [] _ fin = Left (False, dCurr)                 --todas las ramas iteradas
        dfs_multiple m heuristic dCurr dMax (x:xs) visited fin                          --iterar sobre las ramas
            | isRight thisBranch = thisBranch                                           --Camino encontrado: subir solución
            | isRight (otherBranches) = otherBranches                                   --Camino encontrado en otro vecino
            | otherwise = Left ((pruned1 || pruned2), (max num1 num2))                  --Camino no encontrado: Unificar ramas vecinas

            where 
                thisBranch = dfs_sgle m heuristic dCurr dMax visited x fin              --Profundizar en tu rama
                otherBranches = dfs_multiple m heuristic dCurr dMax xs visited fin      --Recorrer (recursivamente) rama de los vecinos
                (pruned1, num1) = fromLeft (False, 0) thisBranch
                (pruned2, num2) = fromLeft (False, 0) otherBranches
                            

--Cuidado: si sabemos que hay camino todo es mucho más fácil. No hay que hacer ciertas comprobaciones de si ha habido 
--podas y seguir aumentando (siempre habría que seguir aumentando)
--Sería buena idea: comprobar que fin tenga adyacentes
