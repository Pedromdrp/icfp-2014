module LoadConfig where

import Datastructures
import LCGen

configToStates :: Configuration -> [State]
configToStates cfg = do
                seed <- configSourceSeeds cfg
                let (u : us) = genUnits (configSourceLength cfg) seed
                return $ State u brd us
        where
                numUnits = length (configUnits cfg)
                cunits = map (centerUnit w) (configUnits cfg)
                getUnit unum = cunits !! (unum `mod` numUnits)
                genUnits l s = take l $ map getUnit (lcgen s)
                w = configWidth cfg
                h = configHeight cfg
                brd = boardFillAll (emptyBoard w h) (configFilled cfg)

centerUnit :: Int -> Unit -> Unit
-- ^Translate the given unit to its starting position
-- TODO: IMPLEMENT THIS!
centerUnit _ u = u
