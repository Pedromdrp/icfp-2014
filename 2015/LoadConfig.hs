module LoadConfig where

import Datastructures
import LCGen
import Moves
import AI

configToStates :: Configuration -> [State]
configToStates cfg = do
                seed <- configSourceSeeds cfg
                let (u : us) = genUnits (configSourceLength cfg) seed
                return $ State u brd us []
        where
                numUnits = length (configUnits cfg)
                cunits = map (centerUnit w) (configUnits cfg)
                getUnit unum = cunits !! (unum `mod` numUnits)
                genUnits l s = take l $ map getUnit (lcgen s)
                w = configWidth cfg
                h = configHeight cfg
                brd = boardFillAll (emptyBoard w h) (configFilled cfg)

makeGUnit :: Unit -> GUnit
makeGUnit u = GUnit u (getSymmetryAngles u) 0

centerUnit :: Int -> Unit -> GUnit
centerUnit w u@(Unit c p) = makeGUnit $ addUnit u (offset - minWidth) (-minHeight)
  where minHeight = minimum (map cellY c)
        minWidth = minimum (map cellX c)
        width = maximum (map cellX c) - minWidth + 1
        offset = quot (w - width) 2
