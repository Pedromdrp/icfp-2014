{-# LANGUAGE DeriveGeneric #-}
module Datastructures where

import GHC.Generics
-- import qualified Data.Vector as V

newtype Cell = Cell (Int, Int) deriving (Show, Generic)

cellX :: Cell -> Int
cellX (Cell (x, y)) = x

cellY :: Cell -> Int
cellY (Cell (x, y)) = y

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
        boardBits :: Integer
        }


boardLookup :: Board -> Cell -> Bool
boardLookup (Board w bs) c = testBit bs ((cellX c) + (cellY c) * w)

boardFill :: Board -> Cell -> Board
boardFill (Board w bs) c = Board w $ setBit bs ((cellX c) + (cellY c) * w)

boardFillAll :: Board -> [Cell] -> Board
boardFillAll = foldl boardFill

data State = State {
        stateUnit :: Unit,
        board :: Board,
        source :: [Unit]
        }
