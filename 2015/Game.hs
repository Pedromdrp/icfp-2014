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

isDownMove :: Move -> Bool
isDownMove (Move SW) = True
isDownMove (Move SE) = True
isDownMove _ = False


data GameOver = EmptySource Int | InvalidSpawn Int | Revisit deriving (Eq,Ord,Show)

doMove :: Move -> State -> Either GameOver State
-- ^Update the state according to a move
-- Try the move
--   if it's valid, do it
--   if it's not then lock
--      try to eliminate lines
--      spawn the next unit (if available)
--      if no such unit or invalid state on spawning then return nothing
doMove mv st = if isValidState st' then 
		if isRevisit then Left Revisit else Right st' else
		if null (source st) then Left (EmptySource newScore) else
		if isValidState st'' then Right st'' else Left (InvalidSpawn newScore)
        where
		history' = if isDownMove mv then [] else (gUnitPos (stateGUnit st) : history st)
                st' = st {stateGUnit = moveUnit mv (stateGUnit st), history = history'}
		isRevisit = elem (gUnitPos (stateGUnit st')) history'
                (unit' : source') = source st
		(board'', ls) = boardClearLines (boardFillAll (board st) (unitMembers (stateUnit st)))
		points = (length $ unitMembers $ stateUnit st) + 50 * (1 + ls) * ls
		lineBonus = if lsOld st > 1 then ((lsOld st - 1) * points) `div` 10 else 0
		newScore = currentScore st + points + lineBonus
                st'' = st {
                        stateGUnit = unit',
                        board = board'',
                        source = source',
			history = [],
			currentScore = newScore,
			lsOld = ls
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
