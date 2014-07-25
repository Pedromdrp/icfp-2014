{-# LANGUAGE DeriveFunctor #-}
module Assembly where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Control.Monad.State
import Control.Arrow

type Literal = Integer

data Instr a
        = LDC Literal
        | LD Literal Literal
        | ADD
        | SUB
        | MUL
        | DIV
        | CEQ
        | CGT
        | CGTE
        | ATOM
        | CONS
        | CAR
        | CDR
        | SEL a a
        | JOIN
        | LDF a
        | AP Literal
        | RTN
        | DUM Literal
        | RAP Literal
        | STOP
        | TSEL a a
        | TAP Literal
        | TRAP Literal
        | ST Literal Literal
        | DBUG
        | BRK
        deriving (Eq, Show, Functor)

newtype MachineCode = MC [Instr Integer]
instance Show MachineCode where
        show (MC l) = intercalate "\n" $ map show l


newtype Assembly a = Assembly (Map a [Instr a])

instance Show a => Show (Assembly a) where
        show (Assembly m) = intercalate "\n" $ map show $ Map.toList m


assemble :: Assembly Integer -> MachineCode
assemble (Assembly m) = MC ass
        where
                codes = Map.toAscList m
                purecodes :: [[Instr Integer]]
                purecodes = map snd codes
                offs = scanl1 (+) $ map length purecodes
                offsMap = Map.fromAscList $ zipWith (\ (x, y) z -> (x, z)) codes (0:offs)
                getOffs x = toInteger $ Map.findWithDefault (error $ "assemble: Invalid label " ++ show x) x offsMap
                fgos :: Instr Integer -> Instr Integer
                fgos = fmap getOffs
                ass = concatMap (map fgos) purecodes


emptyAssembly = Assembly Map.empty


type AM = State (Integer, Assembly Integer)

freshLabel :: AM Integer
freshLabel = do
                (n, a) <- get
                put (n + 1, a)
                return n

setCode :: Integer -> [Instr Integer] -> AM ()
setCode l c = do
                (n, Assembly a) <- get
                put (n, Assembly (Map.insert l c a))


runAM :: AM a -> (a, Assembly Integer)
runAM s = let (res, (_, ass)) = runState s (0, emptyAssembly) in (res, ass)

execAM :: AM a -> Assembly Integer
execAM = snd . runAM

testAssembler = assemble $ execAM $ do
        main <- freshLabel
        body <- freshLabel
        setCode main [LDC 21, LDF body, AP 1, RTN]
        setCode body [LD 0 0, LD 0 0, ADD, RTN]


-- Replace the following instruction pairs with single instructions
-- SEL; JOIN -> TSEL
-- AP; RTN -> TAP
-- RAP; RTN -> TRAP
tailOpt :: Assembly a -> Assembly a
tailOpt (Assembly m) = Assembly (fmap optRoutine m)
        where
                optRoutine (SEL x y: JOIN : []) = [TSEL x y]
                optRoutine (AP x : RTN : []) = [TAP x]
                optRoutine (RAP x : RTN : []) = [TRAP x]
                optRoutine (x : xs) = x : optRoutine xs
                optRoutine [] = []

-- Rewrite:
--   SEL a b; RTN
--   a: ...; JOIN
--   b: ...; JOIN
-- to
--   TSEL a b
--   a: ...; RTN
--   b: ...; RTN
tailOpt1 :: (Ord a) => Assembly a -> Assembly a
tailOpt1 (Assembly m) = Assembly (opt (Map.keys m) m)
        where
                opt [] mm = mm
                opt (k : ks) mm = let c = Map.findWithDefault undefined k mm in
                        case optSELRTN c of
                                (Just (c', (x, y))) -> opt (x : y : ks)
                                                (Map.insert k c' $ Map.adjust adj x $ Map.adjust adj y mm)
                                Nothing -> opt ks mm
                optSELRTN [] = Nothing
                optSELRTN (SEL x y : RTN : []) = Just ([TSEL x y], (x, y))
                optSELRTN (x : xs) = liftM (first (x :)) $ optSELRTN xs
                adj l = init l ++ [RTN]


