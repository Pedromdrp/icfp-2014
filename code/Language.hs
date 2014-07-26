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
  | ABefore Var Exp Exp		-- Assign first expression to variable, compute and return second
  | AAfter Exp Var Exp		-- Compute first, assign second, return first
  | Trace Exp Exp		-- Print first, return second
  deriving (Show, Eq)

mkList :: [Exp] -> Exp
mkList [] = Num 0
mkList (x : xs) = Pair x (mkList xs)

ifNZ test tt ff = IfZ test ff tt

(v, e1) #> e2 = ABefore v e1 e2
e1 #< (v, e2) = AAfter e1 v e2
infixr 5 #>
