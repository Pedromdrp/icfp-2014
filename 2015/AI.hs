module AI where
import Datastructures
import Moves
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

{--
dfs :: State -> [Move] -> (Int, [Move])
dfs st mv =
  do succ <- map (\m -> (doMove m st, mv ++ [m])) getMoves
     

Bah I am stopping now
getUnitMoves :: Either State -> [Move]
getUnitMoves st =
  do succ <- map (\m -> (doMove m st, [m])) getMoves
     
     (entropy1 (board s))
  where helper s m = maximum ()
  do successors <- foldl (\(x, y) xs -> case x of
                                           Left e -> xs
                                           Right s -> (s, y))
                         []
                         
     return if null successors
              then (0, mv)
              else maximum $ map successors

case (doMove m s) of
                        Left e -> print e
                        Right s' -> mainLoop s'
--}