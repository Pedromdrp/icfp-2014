module Language where

data Op = Add | Sub | Mul | Div | CEq | CGt | CGe | CLt | CLe deriving (Show, Eq)

type Var = String

data Exp
  = Var String
  | Num Integer
  | App Exp [Exp]
  | Lam [Var] Exp
  | Let [(Var, Exp)] Exp
  | BOp Exp Op Exp
  | IfZ Exp Exp Exp
  | Pair Exp Exp
  | Fst  Exp
  | Snd  Exp
  | IsAtom Exp
  deriving (Show, Eq)

mkList :: [Exp] -> Exp
mkList [] = Num 0
mkList (x : xs) = Pair x (mkList xs)

ifNZ test tt ff = IfZ test ff tt
