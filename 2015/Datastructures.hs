{-# LANGUAGE DeriveGeneric #-}
module Datastructures where

import GHC.Generics
import Data.List
import Data.Bits
-- import qualified Data.Vector as V

data Cell = Cell {
        cellX :: Int,
        cellY :: Int
        } deriving (Show, Eq)

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
boardLookup (Board w _ bs) c = testBit bs ((cellX c) + (cellY c) * w)

boardFill :: Board -> Cell -> Board
boardFill (Board w h bs) c = Board w h $ setBit bs ((cellX c) + (cellY c) * w)

boardFillAll :: Board -> [Cell] -> Board
boardFillAll = foldl boardFill

data State = State {
        stateUnit :: Unit,
        board :: Board,
        source :: [Unit]
        }

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

