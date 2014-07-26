module Library where
import Language
import Compile

x .+ y = BOp x Add y
x .- y = BOp x Sub y
x .* y = BOp x Mul y
x ./ y = BOp x Div y
x .= y = BOp x CEq y
x .< y = BOp x CLt y

fn $. arg = App (Var fn) arg

listMatch :: Exp -- ^ List
        -> Exp                  -- ^ Empty list
        -> (Exp -> Exp -> Exp)  -- ^ x : xs
        -> Exp
listMatch list base rec = IfZ (IsAtom list) (rec (Fst list) (Snd list)) base

-- nth (index, list): return the index-th item in the list
--      0-based; does not check for length overrun
lib_nth = Lam ["index", "list"] $
        IfZ (Var "index")
                (Fst (Var "list"))
                (App (Var "nth")
                        [Var "index" .- Num 1,
                        Snd (Var "list")])
-- replace (index, value, list) : return the list modified by replacing the item at the index with the new value
lib_replace = Lam ["idx", "val", "l"] $
        listMatch "l"
                (Num 0)
                (\hd tl -> IfZ (Var "idx") (Pair (Var "val") tl) (Pair hd ("replace" $. [(Var "idx"), (Var "val"), tl])))
-- lengthAcc (list, acc): accumulating length function
lib_lengthAcc = Lam ["list", "acc"] $
        IfZ (IsAtom (Var "list"))
                (App (Var "lengthAcc") [Snd (Var "list"), Var "acc" .+ Num 1])
                (Var "acc")
-- length (list): length function
lib_length = Lam ["list"] $ App (Var "lengthAcc") [Var "list", Num 0]

-- map (f, list) : map function f over list
lib_map = Lam ["f", "list"] $
        IfZ (IsAtom (Var "list"))
                (Pair (App (Var "f") [Fst (Var "list")])
                        (App (Var "map") [Var "f", Snd (Var "list")]))
                (Num 0)

-- concat (l1, l2) : concatenate two lists
lib_concat = Lam ["l1", "l2"] $
        IfZ (IsAtom (Var "l1"))
                (Pair (Fst (Var "l1"))
                        (App (Var "concat") [Snd (Var "l1"), Var "l2"]))
                (Var "l2")

-- filter (f, l) : filter list l to those values for which f returns true
lib_filter = Lam ["f", "l"] $
        IfZ (IsAtom (Var "l"))
                (IfZ (App (Var "f") [Fst (Var "l")])
                        (App (Var "filter") [Var "f", Snd (Var "l")])
                        (Pair (Fst (Var "l"))
                                (App (Var "filter") [Var "f", Snd (Var "l")])))
                (Num 0)

-- reverseAcc (l, r) : reverse list l, concatenating with list r
lib_reverseAcc = Lam ["l", "r"] $
        IfZ (IsAtom (Var "l"))
                (App (Var "reverseAcc") [Snd (Var "l"), Pair (Fst (Var "l")) (Var "r")])
                (Var "r")
-- reverse (list) : reverse the list
lib_reverse = Lam ["list"] $ App (Var "reverseAcc") [Var "list", Num 0]

lib_foldl = Lam ["f", "cur", "l"] $
        listMatch (Var "l") (Var "cur")
                (\hd tl -> "foldl" $. [Var "f", "f" $. [Var "cur", hd], tl])

-- find (val, list) : Find the index of (integer) value in the list.
--      Return -1 if not found.
lib_find = Lam ["val", "list"] $
        "findAcc" $. [Var "val", Var "list", Num 0]
lib_findAcc = Lam ["val", "list", "pos"] $
        listMatch (Var "list") (Num -1)
                (\hd tl -> ifNZ (Var "val" .= hd) (Var "pos")
                        ("findAcc" $. [Var "val", tl, Var "pos" .+ Num 1]))

withLib = Let [("nth",lib_nth),
                ("replace",lib_replace),
                ("lengthAcc",lib_lengthAcc),
                ("length",lib_length),
                ("map",lib_map),
                ("concat",lib_concat),
                ("filter",lib_filter),
                ("reverseAcc",lib_reverseAcc),
                ("reverse",lib_reverse),
                ("foldl",lib_foldl),
                ("findAcc",lib_findAcc),
                ("find",lib_find)]

filterTest = withLib $ App (Var "filter") [
                (Lam ["x"] ((Var "x" ./ Num 2) .= (Num 1))),
                mkList [Num x | x <- [0..5]]]

mapTest = withLib $ App (Var "map") [
                (Lam ["x"] (Var "x" .+ Num 2)),
                mkList [Num x | x <- [0..3]]]

reverseTest = withLib $ App (Var "reverse") [mkList [Num x | x <- [0..3]]]

concatTest = withLib $ App (Var "concat") (map (mkList . map Num) [[0,1], [3,2]])

foldlTest = withLib $ "foldl" $. [Lam ["x", "y"] (Var "x" .+ Var "y"),
                                Num 0, mkList [Num x | x <- [0..4]]]
                
