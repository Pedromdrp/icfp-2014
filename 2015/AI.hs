module AI(equalUnit, getSymmetryAngles, dfs, findMove) where
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

updateTransform :: Transform -> Move -> Transform
updateTransform (Transform te se cw) (Move E) = Transform (te + 1) se cw
updateTransform (Transform te se cw) (Move W) = Transform (te - 1) se cw
updateTransform (Transform te se cw) (Move SE) = Transform te (se + 1) cw
updateTransform (Transform te se cw) (Move SW) = Transform (te - 1) (se + 1) cw
updateTransform (Transform te se cw) (Rotate CW) = Transform te se (cw + 1)
updateTransform (Transform te se cw) (Rotate CCW) = Transform te se (mod (cw - 1) 6)

neighbours :: (State, Transform, [Move]) -> Set.Set (State, Transform, [Move])
neighbours (st, t, mv) =
  Set.fromList $ map (\(s, m) -> case s of
                                   Right s' -> (s', updateTransform t m, mv ++ [m])) next
  where succ = map (\m -> (doMove m st, m)) getMoves
        next = filter (\(s, m) -> case s of
                                    Left _ -> False
                                    Right _ -> True) succ

distanceNeighbours :: (State, Transform, [Move]) -> (State, Transform, [Move]) -> Int
distanceNeighbours _ _ = 1

heuristic :: (State, Transform, [Move]) -> Int
heuristic (st, t, mv) = (abs (transformE t)) + (transformSE t) +  (transformCW t)

isGoal :: (State, Transform, [Move]) -> Bool
isGoal (st, t, mv) = (transformE t) == 0 && (transformSE t) == 0 && (transformCW t) == 0

findMove :: State -> Transform -> [Move]
findMove st t = case moves of
                  Nothing -> []
                  Just xs -> m
                               where (s, t', m) = last xs 
  where moves = aStar neighbours distanceNeighbours heuristic isGoal (st, t, [])
