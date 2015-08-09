module AI where
import Datastructures
import Moves
import Game
import Entropy
import Data.List

-- ^Tests if two units with the same pivot are equivalent
equalUnit :: Unit -> Unit -> Bool
equalUnit (Unit c1 p1) (Unit c2 p2) = (p1 == p2) && (sort c1 == sort c2)

-- ^Get list of rotation angles that yield an equivalent unit
getSymmetryAngles :: Unit -> Int
getSymmetryAngles u = getSymmetryAnglesAux u [1, 2, 3]
  where getSymmetryAnglesAux un [] = 6
        getSymmetryAnglesAux un (x:xs) = if (equalUnit u un2) then x else (getSymmetryAnglesAux un2 xs)
          where un2 = rotateACWUnit un

dfs :: State -> [Move] -> Int -> (Int, State, [Move])
dfs st mv score =
  if newScore > score
    then (entropy1 (board st), st, mv)
    else minimum opt
  where newScore = currentScore st
        succ = map (\m -> (doMove m st, m)) getMoves
        next = filter (\(s, m) -> case s of
                                    Left _ -> False
                                    Right _ -> True) succ
        opt = map (\(s, m) -> dfs s (mv ++ [m]) score) (map (\(s, m) -> case s of
                                                                          Right s' -> (s', m)) next)
