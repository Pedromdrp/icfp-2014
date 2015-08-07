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

lib_atom1AndEqual = Lam ["v1", "v2"] $
                IfZ (IsAtom (Var "v1")) (Num 0) (Var "v1" .= Var "v2")


-- Check if we're there by looking for link in direction dir (will be opposite of source direction)
--   if so, return the tree
--   otherwise:
--    for each non-link, available direction except the direction we came from
--      refocus the tree in that direction
--      recursively call
--     if any returns a tree, return that
--      otherwise return 0
lib_linkMove = Lam ["tree", "link", "dir"] $
        Let [
                ("doMove", Lam ["tree", "moveDir"] $
                    ifNZ ("atom1AndEqual" $. ["nth" $. [Var "dir", Var "tree"], Var "link"])
                        (Var "tree")
                        (Let [("up", stepdir 0)] $
                          IfZ (IsAtom $ Var "up") (Var "up") $
                            Let [("right", stepdir 1)] $
                              IfZ (IsAtom $ Var "right") (Var "right") $
                                Let [("down", stepdir 2)] $
                                  IfZ (IsAtom $ Var "down") (Var "down")
                                    (stepdir 3)))] $
                "doMove" $. [Var "tree", Num (-1)]
    where
        field 0 e = Fst e
        field n e = field (n - 1) (Snd e)
        upd tpl 0 v = Pair v (Snd tpl)
        upd tpl n v = Pair (Fst tpl) (upd (Snd tpl) (n - 1) v)
        opd d = (d + 2) `mod` 4
        stepdir d = Trace (Num d) $
                -- First check we're not going backwards
                ifNZ (Var "moveDir" .= Num (opd d)) (Num 0) $
                Let [("treed", field d (Var "tree"))] $
                ifNZ (IsAtom $ Var "treed")
                        -- No tree in that direction, so return 0
                        (Num 0)
                        -- Otherwise refocus and recurse
                        ("doMove" $. [
                                (upd (Var "treed") (opd d) (upd (Var "tree") d (Num (-1)))),
                                Num d])


withMT = withLib . Let [("mapToLM", lib_mapToLM),
                        ("mapToLMAcc", lib_mapToLMAcc),
                        ("lookupXY", lib_lookupXY),
                        ("replaceXY", lib_replaceXY),
                        ("cDir", lib_cDir),
                        ("mtInitPayload", lib_mtInitPayload),
                        ("mtInit", lib_mtInit),
                        ("atom1AndEqual", lib_atom1AndEqual),
                        ("linkMove", lib_linkMove)]

testMaze :: [[Integer]]
testMaze = map (map (read . (:[]))) ["000000000","011101110","010101010","011151110","010101010","011101110","000000000"]

tmExpr = mkList $ map (mkList . map Num) testMaze

replaceTest = withMT $ "replaceXY" $. [tmExpr, Pair (Num 0) (Num 1), Num 888]

lurepTest = withMT $ "lookupXY" $. ["replaceXY" $. [tmExpr, Pair (Num 0) (Num 1), Num 888], Pair (Num 0) (Num 1)]

myTrace x = App (Lam ["x"] $ Trace (Var "x") (Var "x")) [x]

initTest1 = myTrace $ withMT $ "mtInit" $. [mkList $ map (mkList . map Num) testMaze]

linkTest = myTrace $ withMT $ "linkMove" $. [myTrace $ "mtInit" $. [tmExpr], Num 2, Num 0]
