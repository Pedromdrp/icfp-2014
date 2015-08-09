module Moves (addUnit, rotateCWUnit, rotateACWUnit, Direction(..), Rotation(..), Move(..), moveUnit, getMoves, Transform(..), transformUnit) where

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

getMoves :: [Move]
getMoves = [Move E, Move W, Move SE, Move SW, Rotate CW, Rotate CCW]

moveUnit :: Move -> GUnit -> GUnit
moveUnit (Move E) gu = gu {gUnit = addUnit (gUnit gu) 1 0}
moveUnit (Move W) gu = gu {gUnit = addUnit (gUnit gu) (-1) 0}
moveUnit (Move SE) gu = gu {gUnit = addUnit (gUnit gu) 0 1}
moveUnit (Move SW) gu = gu {gUnit = addUnit (gUnit gu) (-1) 1}
moveUnit (Rotate CW) gu = gu {gUnit = rotateCWUnit (gUnit gu), guOrientation = (guOrientation gu + 1) `mod` guSymmetryAngle gu}
moveUnit (Rotate CCW) gu = gu {gUnit = rotateACWUnit (gUnit gu), guOrientation = (guOrientation gu + 5) `mod` guSymmetryAngle gu}

data Transform = Transform {
	transformE :: Int,
	transformSE :: Int,
	transformCW :: Int
	} deriving (Show, Ord, Eq)

translateUnit :: Int -> Int -> Unit -> Unit
translateUnit e se (Unit cells pivot) = Unit (map tc cells) (tc pivot)
	where
		tc (Cell x y) = Cell (x + e + (se `div` 2)) (y + se)

transformUnit :: Transform -> GUnit -> GUnit
transformUnit (Transform e se cw) gu =
		gu {
			gUnit = (ntimes cw rotateCWUnit) (translateUnit e se (gUnit gu)),
			guOrientation = (guOrientation gu + cw) `mod` guSymmetryAngle gu
		}
	where
    		ntimes n f x = iterate f x !! n
