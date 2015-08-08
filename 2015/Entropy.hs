module Entropy(
	entropy1
	) where

import Datastructures
import Data.Bits

filledHexes :: Board -> Int
filledHexes = popCount . boardBits

nw :: Cell -> Cell
nw (Cell x y) = if y `mod` 2 == 0 then Cell (x-1) (y-1) else Cell x (y-1)
ne :: Cell -> Cell
ne (Cell x y) = if y `mod` 2 == 0 then Cell x (y-1) else Cell (x+1) (y-1)
e :: Cell -> Cell
e (Cell x y) = Cell (x+1) y
w :: Cell -> Cell
w (Cell x y) = Cell (x-1) y
sw :: Cell -> Cell
sw (Cell x y) = if y `mod` 2 == 0 then Cell (x-1) (y+1) else Cell x (y+1)
se :: Cell -> Cell
se (Cell x y) = if y `mod` 2 == 0 then Cell x (y+1) else Cell (x+1) (y+1)

{-
 - If a hexagon is filled, it has entropy +5.
 - If it is empty then it has entropy
 - +1 for each filled/off-board SE/SW cell
 - +3 for each filled/off-board E/W cell
 - +8 for each filled/horizontally-off-board NE/NW cell
 - a hexagon's entropy is multiplied by the number of lines from the bottom it is
 -}
entropy1 :: Board -> Int
entropy1 brd = sum (map entropyLine [0..boardHeight brd])
	where
		entropyLine l = (boardHeight brd - l) * sum [entropyHex x l | x <- [0..boardWidth brd - 1]]
		entropyHex x y = let c = Cell x y in if boardLookup brd c then 5 else
			(if boardLookupBounds brd (w c) then 3 else 0)
			+ (if boardLookupBounds brd (e c) then 3 else 0)
			+ (if boardLookupBounds brd (sw c) then 1 else 0)
			+ (if boardLookupBounds brd (se c) then 1 else 0)
			+ (if boardLookupBounds brd (ne c) then 8 else 0)
			+ (if boardLookupBounds brd (nw c) then 8 else 0)
			
