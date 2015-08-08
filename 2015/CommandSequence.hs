module CommandSequence where

import Moves

moveToCommand :: Move -> Char
moveToCommand (Move W) = 'p'
moveToCommand (Move E) = 'b'
moveToCommand (Move SW) = 'a'
moveToCommand (Move SE) = 'l'
moveToCommand (Rotate CW) = 'd'
moveToCommand (Rotate CCW) = 'k'

movesToCommands :: [Move] -> String
movesToCommands = map moveToCommand
