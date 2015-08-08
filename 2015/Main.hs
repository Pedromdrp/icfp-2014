import Datastructures
import Parsing
import LoadConfig
import Moves
import Game

main :: IO ()
main = do
        c <- parseFile "qualifiers/problem_0.json"
        let (s : ss) = configToStates c
        mainLoop s
    where
        mainLoop s = do
                print s
                m <- nextMove
                print m
                case (doMove m s) of
                        Left e -> print e
                        Right s' -> mainLoop s'

nextMove :: IO Move
nextMove = do
                c <- getChar
                case charToMove c of
                        Nothing -> nextMove
                        Just m -> return m
