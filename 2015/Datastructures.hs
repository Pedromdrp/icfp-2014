{-# LANGUAGE DeriveGeneric #-}
module Datastructures where

import GHC.Generics
import Data.List
import Data.Bits
-- import qualified Data.Vector as V

data Cell = Cell {
        cellX :: Int,
        cellY :: Int
        } deriving (Show, Eq, Ord)

data Unit = Unit {
        unitMembers :: [Cell],
        unitPivot :: Cell
        } deriving (Show, Generic)

data Configuration = Configuration {
        configID :: Int,
        configUnits :: [Unit],
        configWidth :: Int,
        configHeight :: Int,
        configFilled :: [Cell],
        configSourceLength :: Int,
        configSourceSeeds :: [Int]
        } deriving (Show, Generic)

data Board = Board {
        boardWidth :: Int,
        boardHeight :: Int,
        boardBits :: Integer
        }

emptyBoard :: Int -> Int -> Board
emptyBoard width height = Board width height 0

boardLookup :: Board -> Cell -> Bool
{-# INLINE boardLookup #-}
boardLookup (Board w _ bs) c = testBit bs ((cellX c) + (cellY c) * w)

boardLookupBounds :: Board -> Cell -> Bool
-- ^Lookup, but allow locations that are out of bounds.
-- Locations off the top are considered empty; off other edges they are full
{-# INLINE boardLookupBounds #-}
boardLookupBounds b@(Board w h bs) c@(Cell x y)
	| y < 0 = False
	| y >= h = True
	| x < 0 = True
	| x > w = True
	| otherwise = boardLookup b c

boardFill :: Board -> Cell -> Board
boardFill (Board w h bs) c = Board w h $ setBit bs ((cellX c) + (cellY c) * w)

boardFillAll :: Board -> [Cell] -> Board
boardFillAll = foldl boardFill

boardClearLines :: Board -> (Board, Int)
-- ^Clear all of the full lines on the board.
-- Move higher lines down
-- Also returns the number of lines cleared
boardClearLines (Board w h brd) = (Board w h b', ls)
	where
		(b', ls) = clear h brd 0
		clear n b acc
			| n < 0 = (b, acc)
			| b .&. mask n == mask n = clear n
				((b .&. (complement $ 2^((n+1)*w) - 1)) .|.
				(shiftL (b .&. (2^(n*w)-1)) w)) (acc + 1)
			| otherwise = clear (n - 1) b acc
		baseMask = 2^w - 1
		mask n = baseMask * 2^(n * w)

data GUnit = GUnit {
	gUnit :: Unit,
	guSymmetryAngle :: Int,
	guOrientation :: Int -- always in [0..guSymmetryAngle-1]
	}

data OldPos = OldPos {
	opColumn :: Int,
	opOrientation :: Int
	} deriving (Eq, Ord)

gUnitPos :: GUnit -> OldPos
gUnitPos gu = OldPos ((cellX . unitPivot . gUnit) gu) (guOrientation gu)

data State = State {
        stateGUnit :: GUnit,
        board :: Board,
        source :: [GUnit],
	history :: [OldPos], -- the positions that the current unit has been in on the current row
	currentScore :: Int,
	lsOld :: Int
        }
stateUnit :: State -> Unit
stateUnit = gUnit . stateGUnit

makeState :: Board -> [GUnit] -> State
makeState b (u:us) = State u b us [] 0 0
makeState _ [] = error "makeState with empty source!"

showBoardCell :: Board -> Cell -> String
showBoardCell b c = if boardLookup b c then "XXX" else "   "

showGrid :: Int -> Int -> (Cell -> String) -> String
showGrid w h sh = concatMap line [0..h-1] ++ if h `mod` 2 == 0 then oddDiags else evenDiags
        where 
                oddDiags :: String
                oddDiags = concat (replicate w " / \\") ++ " /\n"
                evenDiags = concat (replicate w " \\ /")  ++ " \\\n"
                line :: Int -> String
                line y = (if y `mod` 2 == 0 then oddDiags else evenDiags ++ "  ")
                        ++ concat ["|" ++ sh (Cell x y) | x <- [0..w-1]] ++ "|\n"

instance Show Board where
        show b = showGrid (boardWidth b) (boardHeight b) (showBoardCell b)

instance Show State where
        show st = showGrid (boardWidth b) (boardHeight b) showCell
                where
                        b = board st
                        u = stateUnit st
                        showCell c = case (elem c (unitMembers u), c == unitPivot u, boardLookup b c) of
                                (True, _, True) -> "###"
                                (False, False, False) -> "   "
                                (False, False, True) -> "XXX"
                                (False, True, False) -> " . "
                                (False, True, True) -> "X.X"
                                (True, False, False) -> "OOO"
                                (True, True, False) -> "O.O"

