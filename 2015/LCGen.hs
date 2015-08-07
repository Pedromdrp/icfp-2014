import Data.Bits

multiplier = 1103515245
increment = 12345

lcgen :: Int -> [Int]
lcgen seed = rn : lcgen nextseed
        where
                rn = (shiftR seed 16) .&. 0x7FFF
                nextseed = (seed * multiplier + increment) .&. 0xFFFFFFFF
