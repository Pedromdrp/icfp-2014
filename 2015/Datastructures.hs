{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Vector

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

