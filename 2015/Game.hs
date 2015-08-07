module Game where
import Datastructures
import Moves
import LCGen
import Data.Char (toLower)

isValidState :: State -> Bool
-- ^Check if all cells of the unit are on the board and
-- do not intersect occupied locations
isValidState st = all isValidCell (unitMembers (stateUnit st))
        where
                w = boardWidth (board st)
                h = boardHeight (board st)
                isValidCell c@(Cell x y) = x >= 0 && x < w && y >= 0 && y < h
                        && not (boardLookup (board st) c)

doMove :: Move -> State -> Maybe State
-- ^Update the state according to a move
-- Try the move
--   if it's valid, do it
--   if it's not then lock
--      try to eliminate lines
--      spawn the next unit (if available)
--      if no such unit or invalid state on spawning then return nothing
doMove mv st = if isValidState st' then Just st' else
                if not (null (source st)) && isValidState st'' then Just st'' else Nothing
        where
                st' = st {stateUnit = moveUnit mv (stateUnit st)}
                (unit' : source') = source st
                st'' = st {
                        stateUnit = unit',
                        board = boardClearLines (boardFillAll (board st) (unitMembers (stateUnit st))),
                        source = source'
                        }
                

charToMove :: Char -> Maybe Move
charToMove = charToMove' . toLower

charToMove' 'p' = Just $ Move W
charToMove' '\'' = Just $ Move W
charToMove' '!' = Just $ Move W
charToMove' '.' = Just $ Move W
charToMove' '0' = Just $ Move W
charToMove' '3' = Just $ Move W
charToMove' 'b' = Just $ Move E
charToMove' 'c' = Just $ Move E
charToMove' 'e' = Just $ Move E
charToMove' 'f' = Just $ Move E
charToMove' 'y' = Just $ Move E
charToMove' '2' = Just $ Move E
charToMove' 'a' = Just $ Move SW
charToMove' 'g' = Just $ Move SW
charToMove' 'h' = Just $ Move SW
charToMove' 'i' = Just $ Move SW
charToMove' 'j' = Just $ Move SW
charToMove' '4' = Just $ Move SW
charToMove' 'l' = Just $ Move SE
charToMove' 'm' = Just $ Move SE
charToMove' 'n' = Just $ Move SE
charToMove' 'o' = Just $ Move SE
charToMove' ' ' = Just $ Move SE
charToMove' '5' = Just $ Move SE
charToMove' 'd' = Just $ Rotate CW
charToMove' 'q' = Just $ Rotate CW
charToMove' 'r' = Just $ Rotate CW
charToMove' 'v' = Just $ Rotate CW
charToMove' 'z' = Just $ Rotate CW
charToMove' '1' = Just $ Rotate CW
charToMove' 'k' = Just $ Rotate CCW
charToMove' 's' = Just $ Rotate CCW
charToMove' 't' = Just $ Rotate CCW
charToMove' 'u' = Just $ Rotate CCW
charToMove' 'w' = Just $ Rotate CCW
charToMove' 'x' = Just $ Rotate CCW
charToMove' _ = Nothing
