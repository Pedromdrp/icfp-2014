module Language where

data Op = Add | Sub | Mul | Div | CEq | CGt | CGe | CLt | CLe

data Exp
  = Var String
  | Num Integer
  | App Exp [Exp]
  | Lam [Var] Exp
  | Let [Var * Exp] Exp
  | BOp Exp Op Exp
  | IfZ Exp Exp Exp
  | Pair Exp Exp
  | Fst  Exp
  | Snd  Exp
