module Library where
import Language
import Compile

x .+ y = BOp x Add y
x .- y = BOp x Sub y

-- nth (index, list): return the index-th item in the list
--      0-based; does not check for length overrun
lib_nth = Lam ["index", "list"] $
        IfZ (Var "index")
                (Fst (Var "list"))
                (App (Var "nth")
                        [Var "index" .- Num 1,
                        Snd (Var "list")])

lib_lengthAcc = Lam ["list", "acc"] $
        IfZ (IsAtom (Var "list"))
                (App (Var "lengthAcc") [Snd (Var "list"), Var "acc" .+ Num 1])
                (Var "acc")

lib_length = Lam ["list"] $ App (Var "lengthAcc") [Var "list", Num 0]
        


withLib = Let [("nth",lib_nth),
                ("lengthAcc",lib_lengthAcc),
                ("length",lib_length)]
