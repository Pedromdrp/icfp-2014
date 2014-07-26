module Minimax where
import Language
import Compile
import Library

x .|| y = (IfZ (x) (Num 1) (IfZ (y) (Num 1) (Num 0)))

maxLoop = Lam ["node", "best", "moves", "depth"] $
        IfZ (IsAtom (Var "moves"))
                (Var "best")
                (Let [("val", App (Var "minimax") [App (Var "play") [Var "node", Fst (Var "moves")], Var "depth" .- Num 1, Num 1])]
                        (App (Var "maxLoop") [Var "node", IfZ (BOp (Var "val") CGt (Var "best")) (Var "val") (Var "best"), Snd (Var "moves"), Var "depth"]))

minLoop = Lam ["node", "best", "moves", "depth"] $
        IfZ (IsAtom (Var "moves"))
                (Var "best")
                (Let [("val", App (Var "minimax") [App (Var "play") [Var "node", Fst (Var "moves")], Var "depth" .- Num 1, Num 0])]
                        (App (Var "minLoop") [Var "node", IfZ (BOp (Var "val") CLt (Var "best")) (Var "val") (Var "best"), Snd (Var "moves"), Var "depth"]))

minimax = Lam ["node", "depth", "maxPlayer"] $
        Let [("maxLoop", maxLoop), ("minLoop", minLoop)]
                (IfZ ((BOp (Var "index") CEq (Num 0)) .|| (App (Var "terminal") [Var "node"]))
                        (App (Var "heuristic") [Var "node"])
                        (Let [("list", App (Var "moves") [Var "node"])]
                                (IfZ (Var "maxPlayer")
                                        (App (Var "maxLoop") [Var "node", Num (-1000000), Var "list", Var "depth"])
                                        (App (Var "minLoop") [Var "node", Num 1000000, Var "list", Var "depth"]))))

findMoveLoop = Lam ["node", "best", "moves", "depth"] $
        IfZ (IsAtom (Var "moves"))
                (Var "best")
                (Let [("val", App (Var "minimax") [App (Var "play") [Var "node", Fst (Var "moves")], Var "depth" .- Num 1, Num 1])]
                        (App (Var "maxLoop") [Var "node", IfZ (BOp (Var "val") CGt (Snd (Var "best"))) (Pair (Fst (Var "moves")) (Var "val")) (Var "best"), Snd (Var "moves"), Var "depth"]))



findMove = Lam ["node", "depth"] $
        Let [("findMoveLoop", findMoveLoop), ("list", App (Var "move") [Var "node"])]
                (Fst (App (Var "findMoveLoop") [Var "node", Pair (Num (-1)) (Num (-1000000)), Var "list", Var "depth"]))

