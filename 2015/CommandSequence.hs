module CommandSequence where

import Moves
import Data.Char (toLower)
import Data.Maybe
import GHC.Exts
import qualified Data.Algorithms.KMP as KMP

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

stringToMoves :: String -> [Move]
stringToMoves = map (fromJust . charToMove)

moveToCommand :: Move -> Char
moveToCommand (Move W) = 'p'
moveToCommand (Move E) = 'b'
moveToCommand (Move SW) = 'a'
moveToCommand (Move SE) = 'l'
moveToCommand (Rotate CW) = 'd'
moveToCommand (Rotate CCW) = 'k'

movesToCommands :: [Move] -> String
movesToCommands = map moveToCommand

powerMovesToCommands :: [String] -> [Move] -> String
powerMovesToCommands power = m2c tables
	where
		power' = sortWith ((0-) . length) power
		tables = [(KMP.build $ stringToMoves s, s) | s <- power']
		m2c [] ms = movesToCommands ms
		m2c tbls@((t,s):ts) ms = case KMP.match t ms of
			[] -> m2c ts ms
			(p:_) -> m2c ts (take p ms) ++ s ++ m2c tbls (drop (p+(length s)) ms)
