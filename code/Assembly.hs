module Assembly where
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

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


newtype Assembly a = Assembly (Data.Map a [Instr a])


type AM a = ()

freshLabel :: AM Integer
freshLabel = undefined
setCode :: Integer -> [Instr Integer] -> AM ()
setCode = undefined

