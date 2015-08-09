module AI(equalUnit, getSymmetryAngles, dfs) where
import Datastructures
import Moves
import Game
import Entropy
import Data.List
import Data.Graph.AStar
import qualified Data.Set as Set

-- ^Tests if two units with the same pivot are equivalent
equalUnit :: Unit -> Unit -> Bool
equalUnit (Unit c1 p1) (Unit c2 p2) = (p1 == p2) && (sort c1 == sort c2)

-- ^Get list of rotation angles that yield an equivalent unit
getSymmetryAngles :: Unit -> Int
getSymmetryAngles u = getSymmetryAnglesAux u [1, 2, 3]
  where getSymmetryAnglesAux un [] = 6
        getSymmetryAnglesAux un (x:xs) = if (equalUnit u un2) then x else (getSymmetryAnglesAux un2 xs)
          where un2 = rotateACWUnit un


dfsAux :: State -> [Move] -> Int -> (Int, State, [Move])
dfsAux st mv score =
  if newScore > score
    then (entropy1 (board st), st, mv)
    else case opt of
           [] -> (2^29-1, st, mv)
           xs -> minimum xs
  where newScore = currentScore st
        succ = map (\m -> (doMove m st, m)) getMoves
        next = filter (\(s, m) -> case s of
                                    Left _ -> False
                                    Right _ -> True) succ
        opt = map (\(s, m) -> dfsAux s (mv ++ [m]) score)
                  (map (\(s, m) -> case s of
                                     Right s' -> (s', m)) next)

dfs :: State -> (Int, State, [Move])
dfs st = dfsAux st [] (currentScore st)

-- AStar functions

neighbours :: (State, Transform) -> Set.Set (State, Transform)
neighbours (st, t) = Set.empty

--Move -> State -> Either GameOver State

distanceNeighbours :: (Ord a, Num c) => a -> a -> c
distanceNeighbours _ _ = 1

heuristic :: (State, Transform) -> Int
heuristic (st, t) = (abs (transformE t)) + (transformSE t) +  (transformCW t)

isGoal :: (State, Transform) -> Bool
isGoal (st, t) = (transformE t) == 0 && (transformSE t) == 0 && (transformCW t) == 0
