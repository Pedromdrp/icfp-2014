module Targetting where

import Datastructures
import Game
import Moves
import Entropy
import Data.Maybe
import Data.Bits
import GHC.Exts
import Debug.Trace
import Control.Exception.Base
import Control.Monad

interlock :: [[a]] -> [a]
interlock [] = []
interlock [x] = x
interlock ([]:xs) = interlock xs
interlock ((a:as):xs) = a : (interlock (xs ++ [as]))

generateBaseTargets :: Int -> Int -> GUnit -> [Transform]
-- Generate the possible locations on an empty board for the target
-- Assumes that the unit is already at the top of the board
generateBaseTargets w h gu0 = interlock [gbtr gu' cw | (gu',cw) <- rotations 0 gu0]
	where
		rotations n gur
			| n < guSymmetryAngle gu0 = (gur,n) : rotations (n+1) (moveUnit (Rotate CW) gur)
			| otherwise = []
		mse (Cell x y) = if y `mod` 2 == 0 then Cell x (y+1) else Cell (x+1) (y+1)
		checkTransform t = let tgu = transformUnit t gu0 in all isValidCell (unitMembers (gUnit tgu))
                isValidCell c@(Cell x y) = x >= 0 && x < w && y >= 0 && y < h
                gbtr gu' cw = do
        		let gum = unitMembers $ gUnit gu'
        		se <- reverse [(max 0 (- minimum (map cellY gum)))..h - 1 - maximum (map cellY gum)]
	        	let minE = if se `mod` 2 == 0 then
	        		0 - minimum (map cellX gum) - (se `div` 2)
		            else
        		    	0 - minimum (map (cellX . mse) gum) - (se `div` 2)
	        	let maxE = if se `mod` 2 == 0 then
		        	w - 1 - maximum (map cellX gum) - (se `div` 2)
        		    else
	        	    	w - 1 - maximum (map (cellX . mse) gum) - (se `div` 2)
		        e <- [minE..maxE]
        		-- return $ let t = Transform e se cw in (if not $ checkTransform t then trace (show t ++ " " ++ show (unitPivot (gUnit gu0)) ++ " " ++ show (unitPivot (gUnit (transformUnit t gu0)))) else id) assert (checkTransform t) t
	        	return $ Transform e se cw
                


checkValidEntropy :: State -> Transform -> Maybe (Transform, GUnit, Int)
-- Checks if the transform would intersect existing cells
-- Does not check for out of bounds
-- Returns Nothing if position is invalid
-- Otherwise returns Just (t, transformed unit, entropy)
checkValidEntropy s t = if isValid then Just (t, tunit, (entropy . fst . boardClearLines) (brd { boardBits = (boardBits brd) .|. bpiece })) else Nothing
	where
		brd = board s
		tunit = transformUnit t $ stateGUnit s
		bpiece = boardBits $ boardFillAll (emptyBoard (boardWidth brd) (boardHeight brd))
			(unitMembers $ gUnit $ tunit)
		isValid = (bpiece .&. (boardBits brd)) == 0

validFiltered :: State -> [(Transform, GUnit, Int)]
validFiltered s = mapMaybe (checkValidEntropy s) (let x = (generateBaseTargets w h gu) in {-trace (show x)-} x)
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
			m <- [Move m | m <- [E, W, SE, SW]] ++ 
				case guSymmetryAngle gu0 of
					1 -> []
					2 -> [Rotate CW]
					_ -> [Rotate CW, Rotate CCW]
			let gu' = moveUnit m gu0
			c <- unitMembers (gUnit gu')
			return $ isInvalidCell c
                w = boardWidth (board st)
                h = boardHeight (board st)
                isInvalidCell c@(Cell x y) = x < 0 || x >= w || y < 0 || y >= h
                        || (boardLookup (board st) c)


checkLockable :: State -> (Transform, GUnit, Int) -> Maybe (Transform, GUnit, Int, Move)
checkLockable st (t0, gu0, sc0) = do
                m <- (msum . map Just) $ [Move m | m <- [E, W, SE, SW]] ++ 
				case guSymmetryAngle gu0 of
					1 -> []
					2 -> [Rotate CW]
					_ -> [Rotate CW, Rotate CCW]
                let gu' = moveUnit m gu0
                if or (map isInvalidCell (unitMembers (gUnit gu'))) then
                        return (t0, gu0, sc0, m) else mzero
        where
                w = boardWidth (board st)
                h = boardHeight (board st)
                isInvalidCell c@(Cell x y) = x < 0 || x >= w || y < 0 || y >= h
                        || (boardLookup (board st) c)

transforms1 :: State -> [Transform]
transforms1 st = map (\(t, _, _) -> t) $ filter (\(_, u, _) -> isLockable st u) $ sortWith (\(_, _, e) -> e) $ validFiltered st

transforms2 :: State -> [Transform]
transforms2 st = map (\(t, _, _) -> t) $ sortWith (\(_, _, e) -> e) $ filter (\(_, u, _) -> isLockable st u) $ validFiltered st

transforms3 :: State -> [(Transform, Move)]
transforms3 st = map (\(t, _, _, m) -> (t,m)) $ sortWith (\(_, _, e, _) -> e) $ mapMaybe (checkLockable st) $ validFiltered st
