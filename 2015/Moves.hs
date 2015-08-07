module Moves (unitAdd, unitRotateCW, unitRotateACW) where

import Datastructures

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

cubeRotateCW :: Cube -> Cube
cubeRotateCW (Cube x y z) = Cube (-z) (-x) (-y)

cubeRotateACW :: Cube -> Cube
cubeRotateACW (Cube x y z) = Cube (-y) (-z) (-x)

cubeAdd :: Cube -> Int -> Int -> Int -> Cube
cubeAdd (Cube x y z) xx yy zz = Cube (x + xx) (y + yy) (z + zz)

cellAdd :: Cell -> Int -> Int -> Cell
cellAdd c xx yy = toCell (cubeAdd (toCube c) (cubeX aux) (cubeY aux) (cubeZ aux))
  where aux = toCube (Cell xx yy)

unitAdd :: Unit -> Int -> Int -> Unit
unitAdd (Unit cells pivot) x y = Unit (map (\c -> cellAdd c x y) cells) (cellAdd pivot x y)

unitRotateCW :: Unit -> Unit
unitRotateCW u@(Unit _ (Cell x y)) = unitAdd (Unit (map (toCell . cubeRotateCW . toCube) (unitMembers newUnit)) (unitPivot newUnit)) x y
  where newUnit = unitAdd u (-x) (-y)

unitRotateACW :: Unit -> Unit
unitRotateACW u@(Unit _ (Cell x y)) = unitAdd (Unit (map (toCell . cubeRotateACW . toCube) (unitMembers newUnit)) (unitPivot newUnit)) x y
  where newUnit = unitAdd u (-x) (-y)
