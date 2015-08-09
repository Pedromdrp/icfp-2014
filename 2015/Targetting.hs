module Targetting where

import Datastructures
import Game
import Moves
import Entropy
import Data.Maybe
import Data.Bits
import GHC.Exts

generateBaseTargets :: Int -> Int -> GUnit -> [Transform]
-- Generate the possible locations on an empty board for the target
-- Assumes that the unit is already at the top of the board
generateBaseTargets w h gu0 = do
		(gu',cw) <- rotations 0 gu0
		let gum = unitMembers $ gUnit gu'
		se <- [0..h - 1 - maximum (map cellY gum)]
		let minE = if se `mod` 2 == 0 then
			0 - minimum (map cellX gum) - (se `div` 2)
		    else
		    	0 - minimum (map (cellX . mse) gum) - (se `div` 2)
		let maxE = if se `mod` 2 == 0 then
			w - 1 - maximum (map cellX gum) - (se `div` 2)
		    else
		    	w - 1 - maximum (map (cellX . mse) gum) - (se `div` 2)
		e <- [minE..maxE]
		return $ Transform e se cw
	where
		rotations n gur
			| n < guSymmetryAngle gu0 = (gur,n) : rotations (n+1) (moveUnit (Rotate CW) gur)
			| otherwise = []
		mse (Cell x y) = if y `mod` 2 == 0 then Cell x (y+1) else Cell (x+1) (y+1)

checkValidEntropy :: State -> Transform -> Maybe (Transform, GUnit, Int)
-- Checks if the transform would intersect existing cells
-- Does not check for out of bounds
-- Returns Nothing if position is invalid
-- Otherwise returns Just (t, transformed unit, entropy)
checkValidEntropy s t = if isValid then Just (t, tunit, entropy (brd { boardBits = (boardBits brd) .|. bpiece })) else Nothing
	where
		brd = board s
		tunit = transformUnit t $ stateGUnit s
		bpiece = boardBits $ boardFillAll (emptyBoard (boardWidth brd) (boardHeight brd))
			(unitMembers $ gUnit $ tunit)
		isValid = (bpiece .&. (boardBits brd)) == 0

validFiltered :: State -> [(Transform, GUnit, Int)]
validFiltered s = mapMaybe (checkValidEntropy s) (generateBaseTargets w h gu)
	where
		brd = board s
		w = boardWidth brd
		h = boardHeight brd
		gu = stateGUnit s

isLockable :: State -> GUnit -> Bool
isLockable st gu0 = or db
	where
		db :: [Bool]
		db = do
			m <- getMoves
			let gu' = moveUnit m gu0
			c <- unitMembers (gUnit gu')
			return $ isInvalidCell c
                w = boardWidth (board st)
                h = boardHeight (board st)
                isInvalidCell c@(Cell x y) = x < 0 || x >= w || y < 0 && y >= h
                        || (boardLookup (board st) c)
	
transforms1 :: State -> [Transform]
transforms1 st = map (\(t, _, _) -> t) $ filter (\(_, u, _) -> isLockable st u) $ sortWith (\(_, _, e) -> e) $ validFiltered st

transforms2 :: State -> [Transform]
transforms2 st = map (\(t, _, _) -> t) $ sortWith (\(_, _, e) -> e) $ filter (\(_, u, _) -> isLockable st u) $ validFiltered st
