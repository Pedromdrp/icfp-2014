module Minimax where
import Language
import Compile
import Library

x .|| y = (IfZ (x) (Num 1) (IfZ (y) (Num 1) (Num 0)))

maxLoop = Lam ["best", "list", "depth"] $
        IfZ (IsAtom (Var "list"))
                (Var "best")
                (Let [("val", App (Var "minimax") [Fst (Var "list"), Var "depth" .- Num 1, Num 1])]
                        (App (Var "maxLoop") [IfZ (BOp (Var "val") CGt (Var "best")) (Var "val") (Var "best"), Snd (Var "list"), Var "depth"]))

minLoop = Lam ["best", "list", "depth"] $
        IfZ (IsAtom (Var "list"))
                (Var "best")
                (Let [("val", App (Var "minimax") [Fst (Var "list"), Var "depth" .- Num 1, Num 0])]
                        (App (Var "minLoop") [IfZ (BOp (Var "val") CLt (Var "best")) (Var "val") (Var "best"), Snd (Var "list"), Var "depth"]))



minimax = Lam ["node", "depth", "maxPlayer"] $
        Let [("maxLoop", maxLoop), ("minLoop", minLoop)]
                (IfZ ((BOp (Var "index") CEq (Num 0)) .|| (App (Var "terminal") [Var "node"]))
                        (App (Var "heuristic") [Var "node"])
                        (Let [("list", App (Var "successors") [Var "node"])]
                                (IfZ (Var "maxPlayer")
                                        (App (Var "maxLoop") [Num (-1000000), Var "list", Var "depth"])
                                        (App (Var "minLoop") [Num 1000000, Var "list", Var "depth"]))))

