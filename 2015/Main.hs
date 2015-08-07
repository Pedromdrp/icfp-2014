import Datastructures
import Parsing
import LoadConfig
import Moves
import Game
import Data.Char (toLower)

main :: IO ()
main = do
        c <- parseFile "qualifiers/problem_3.json"
        let (s : ss) = configToStates c
        mainLoop s
    where
        mainLoop s = do
                print s
                m <- nextMove
                print m
                case (doMove m s) of
                        Nothing -> return ()
                        Just s' -> mainLoop s'

nextMove :: IO Move
nextMove = do
                c <- getChar
                case charToMove (toLower c) of
                        Nothing -> nextMove
                        Just m -> return m
