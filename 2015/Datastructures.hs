import Data.Vector

newtype Cell = Cell (Int, Int)

cellX :: Cell -> Int
cellX (Cell (x, y)) = x

cellY :: Cell -> Int
cellY (Cell (x, y)) = y

data Unit = Unit {
        unitMembers :: [Cell],
        unitPivot :: Cell
        }

data Configuration = Configuration {
        configID :: Int,
        configUnits :: [unit],
        configWidth :: Int,
        configHeight :: Int,
        configFilled :: [Cell],
        configSourceLength :: Int,
        configSourceSeeds :: [Int]
        }

