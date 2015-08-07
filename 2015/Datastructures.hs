{-# LANGUAGE DeriveGeneric #-}
module Datastructures where

import GHC.Generics
import Data.Vector

data Cell = Cell {
        cellX :: Int,
        cellY :: Int
        } deriving (Show)

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

-- data State = State {
