module Assembly where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Control.Monad.State

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
        deriving (Eq, Show)

newtype MachineCode = MC [Instr Integer]
instance Show MachineCode where
        show (MC l) = intercalate "\n" $ map show l


newtype Assembly a = Assembly (Map a [Instr a])

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

