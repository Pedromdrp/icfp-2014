module Compile where

import Data.List
import Language
import Assembly

type Env = [[Var]]

findVar' _ [] x = error $ "Undeclared identifier " ++ x
findVar' n (xs : xss) x =
  case elemIndex x xs of
    Just m  -> (n, toInteger m)
    Nothing -> findVar' (n + 1) xss x

findVar :: Env -> String -> (Integer, Integer)
findVar = findVar' 0

compileOp :: [Instr Integer] -> [Instr Integer] -> Op -> [Instr Integer]
compileOp cl cr Add = cl ++ cr ++ [ADD]
compileOp cl cr Sub = cl ++ cr ++ [SUB]
compileOp cl cr Mul = cl ++ cr ++ [MUL]
compileOp cl cr Div = cl ++ cr ++ [DIV]
compileOp cl cr CEq = cl ++ cr ++ [CEQ]
compileOp cl cr CGt = cl ++ cr ++ [CGT]
compileOp cl cr CGe = cl ++ cr ++ [CGTE]
compileOp cl cr CLt = cr ++ cl ++ [CGT]
compileOp cl cr CLe = cr ++ cl ++ [CGTE]

compileAM :: Env -> Exp -> AM [Instr Integer]
compileAM rho (Var x) =
  return [LD fi vi]
  where (fi, vi) = findVar rho x
compileAM rho (Num n) =
  return [LDC n]
compileAM rho (App e es) =
  do cs <- mapM (compileAM rho) es
     c  <- compileAM rho e
     return (concat cs ++ c ++ [AP (toInteger (length cs))])
compileAM rho (Lam xs e) =
  do l <- freshLabel
     c <- compileAM (xs : rho) e
     setCode l (c ++ [RTN])
     return [LDF l]
compileAM rho (Let ves e) =
  do cs <- mapM (compileAM (vs : rho)) es
     l <- freshLabel
     c <- compileAM (vs : rho) e
     setCode l (c ++ [RTN])
     return (DUM n : concat cs ++ [LDF l, RAP n])
       where vs = map fst ves
             es = map snd ves
             n  = toInteger (length ves)
compileAM rho (BOp e1 op e2) =
  do c1 <- compileAM rho e1
     c2 <- compileAM rho e2
     return (compileOp c1 c2 op)
compileAM rho (IfZ e et ef) =
  do test <- compileAM rho e
     lt <- freshLabel
     ct <- compileAM rho et
     setCode lt (ct ++ [JOIN])
     lf <- freshLabel
     cf <- compileAM rho ef
     setCode lf (cf ++ [JOIN])
     return (test ++ [SEL lf lt])
compileAM rho (Pair e1 e2) =
  do c1 <- compileAM rho e1
     c2 <- compileAM rho e2
     return (c1 ++ c2 ++ [CONS])
compileAM rho (Fst e) =
  do c <- compileAM rho e
     return (c ++ [CAR])
compileAM rho (Snd e) =
  do c <- compileAM rho e
     return (c ++ [CDR])

compile e = execAM $ do
  main <- freshLabel
  c <- compileAM [] e
  setCode main (c ++ [RTN])

testCompile = assemble . compile

prog1 = BOp (Num 3) Add (Num 2)
prog2 = Lam ["x"] (BOp (Var "x") Add (Num 2))
prog3 = Let [("fac", Lam ["x"] (IfZ (Var "x") (Num 1) (BOp (Var "x") Mul (App (Var "fac") [BOp (Var "x") Sub (Num 1)]))))] (App (Var "fac") [Num 5])
