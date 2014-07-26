module MazeTree where
import Language
import Library
import Compile





-- Find lambda man's coordinates on the map.
lib_mapToLM = Lam ["tmap"] $ "mapToLMAcc" $. [Var "tmap", Num 0]

lib_mapToLMAcc = Lam ["pmap", "currow"] $
        listMatch (Var "pmap")
                (Num 0) -- This case shouldn't happen!
                (\hd tl -> Let [("x", "find" $. [Num 5, hd])] $
                        ifNZ (Var "x" .= Num (-1))
                                ("mapToLMAcc" $. [tl, Var "currow" .+ Num 1])
                                (Pair (Var "x") (Var "currow")))

lib_lookupXY = Lam ["tmap", "coord"] $
        "nth" $. [Fst (Var "coord"), "nth" $. [Snd (Var "coord"), Var "tmap"]]

lib_replaceXY = Lam ["tmap", "coord", "val"] $
        "update" $. [Snd (Var "coord"), 
                Lam ["row"] $ "replace" $. [Fst (Var "coord"), Var "val", Var "row"],
                Var "tmap"]

cUp c = Pair (Fst c) (Snd c .- Num 1)
cDown c = Pair (Fst c) (Snd c .+ Num 1)
cLeft c = Pair (Fst c .- Num 1) (Snd c)
cRight c = Pair (Fst c .+ Num 1) (Snd c)

moveDir 0 = cUp
moveDir 1 = cRight
moveDir 2 = cDown
moveDir 3 = cLeft

-- cDir (dir, coord) : move the coordinate in the direction indicated
lib_cDir = Lam ["direction", "coord"] $
        ifNZ (Var "direction" .< Num 2)
                (IfZ (Var "direction") (cUp (Var "coord")) (cRight (Var "coord")))
                (ifNZ (Var "direction" .= Num 3) (cDown (Var "coord")) (cLeft (Var "coord")))

-- mtInitPayload (contents) : given the contents, determine the initial payload
lib_mtInitPayload = Lam ["x"] $ Var "x"


lib_mtInit = Lam ["tmap"] $
        Let [
                ("counter", Num (-1)), -- Number each cell we visit. When we link two cells, we use the lower number to identify the link; there can be an up-down link with the same number as a left-right one.
                ("buildTree", Lam ["c", "d", "ocount"] $
                        Let [("cell", "lookupXY" $. [Var "tmap", Var "c"]),
                                ("mycount", Var "counter")] $
                            IfZ (Var "cell")
                                -- It's a wall! 
                                (Num 0) 
                                -- not a wall
                                (ifNZ (Var "cell" .< Num 0)
                                    -- already visited so link
                                    (Num 0 .- (ilMax (Var "cell") (Var "ocount")))
                                    -- not visited
                                    (
                                        -- mark it with the counter
                                        ("tmap", "replaceXY" $. [Var "tmap", Var "c", Var "counter"]) #> 
                                        -- Progress the counter
                                        ("counter", Var "counter" .- Num 1) #> ( -- Trace (Var "tmap") $
                                        -- recurse
                                        quint (cbuildtree 0,
                                                cbuildtree 1,
                                                cbuildtree 2,
                                                cbuildtree 3,
                                                "mtInitPayload" $. [(Var "cell")])))))
        ] $
            "buildTree" $. ["mapToLM" $. [Var "tmap", Num (-1)], Num (-1), Num 0]
    where
                cbuildtree d =
                        ifNZ (Var "d" .= Num ((d + 2) `mod` 4))
                                -- Going back the way we came, so don't recurse this way
                                (Num $ -1)
                                -- Going onward!
                                ("buildTree" $. [moveDir d (Var "c"), Num d, Var "mycount"])
            
withMT = withLib . Let [("mapToLM", lib_mapToLM),
                        ("mapToLMAcc", lib_mapToLMAcc),
                        ("lookupXY", lib_lookupXY),
                        ("replaceXY", lib_replaceXY),
                        ("cDir", lib_cDir),
                        ("mtInitPayload", lib_mtInitPayload),
                        ("mtInit", lib_mtInit)]

testMaze :: [[Integer]]
testMaze = map (map (read . (:[]))) ["000000000","011101110","010101010","011151110","010101010","011101110","000000000"]

tmExpr = mkList $ map (mkList . map Num) testMaze

replaceTest = withMT $ "replaceXY" $. [tmExpr, Pair (Num 0) (Num 1), Num 888]

lurepTest = withMT $ "lookupXY" $. ["replaceXY" $. [tmExpr, Pair (Num 0) (Num 1), Num 888], Pair (Num 0) (Num 1)]

myTrace x = App (Lam ["x"] $ Trace (Var "x") (Var "x")) [x]

initTest1 = myTrace $ withMT $ "mtInit" $. [mkList $ map (mkList . map Num) testMaze]

