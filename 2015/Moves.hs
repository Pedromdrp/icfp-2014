module Moves (addUnit, rotateCWUnit, rotateACWUnit, Direction(..), Rotation(..), Move(..), moveUnit) where

import Datastructures

data Direction = E | W | SE | SW deriving (Eq, Ord, Show)
data Rotation = CW | CCW deriving (Eq, Ord, Show)
data Move = Move Direction | Rotate Rotation deriving (Eq, Ord, Show)

data Cube = Cube {
        cubeX :: Int,
        cubeY :: Int,
        cubeZ :: Int
        } deriving (Show)

toCube :: Cell -> Cube
toCube (Cell x y) = Cube newX (- newX - y) y 
  where newX = x - (quot (y - (rem y 2)) 2)

toCell :: Cube -> Cell
toCell (Cube x y z) = Cell (x + (quot (z - (rem z 2)) 2)) z

rotateCWCube :: Cube -> Cube
rotateCWCube (Cube x y z) = Cube (-z) (-x) (-y)

rotateACWCube :: Cube -> Cube
rotateACWCube (Cube x y z) = Cube (-y) (-z) (-x)

addCube :: Cube -> Int -> Int -> Int -> Cube
addCube (Cube x y z) xx yy zz = Cube (x + xx) (y + yy) (z + zz)

addCell :: Cell -> Int -> Int -> Cell
addCell c xx yy = toCell (addCube (toCube c) (cubeX aux) (cubeY aux) (cubeZ aux))
  where aux = toCube (Cell xx yy)

addUnit :: Unit -> Int -> Int -> Unit
addUnit (Unit cells pivot) x y = Unit (map (\c -> addCell c x y) cells) (addCell pivot x y)

rotateCWUnit :: Unit -> Unit
rotateCWUnit u@(Unit _ (Cell x y)) = addUnit (Unit (map (toCell . rotateCWCube . toCube) (unitMembers newUnit)) (unitPivot newUnit)) x y
  where newUnit = addUnit u (-x) (-y)

rotateACWUnit :: Unit -> Unit
rotateACWUnit u@(Unit _ (Cell x y)) = addUnit (Unit (map (toCell . rotateACWCube . toCube) (unitMembers newUnit)) (unitPivot newUnit)) x y
  where newUnit = addUnit u (-x) (-y)

moveUnit :: Move -> Unit -> Unit
moveUnit (Move E) u = addUnit u 1 0
moveUnit (Move W) u = addUnit u (-1) 0
moveUnit (Move SE) u = addUnit u 0 1
moveUnit (Move SW) u = addUnit u (-1) 1
moveUnit (Rotate CW) u = rotateCWUnit u
moveUnit (Rotate CCW) u = rotateACWUnit u
