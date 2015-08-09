import Datastructures
import Parsing
import LoadConfig
import Moves
import Game
import Entropy
import CommandSequence
import System.Environment
import Data.List
import AI

fileNames :: [String] -> [String]
fileNames [] = []
fileNames (p:v:xs) = (if p == "-f" then [v] else []) ++ fileNames xs

timeLimit :: [String] -> Maybe Int
timeLimit [] = Nothing
timeLimit (p:v:xs) = if p == "-t" then Just (read v :: Int) else timeLimit xs

memoryLimit :: [String] -> Maybe Int
memoryLimit [] = Nothing
memoryLimit (p:v:xs) = if p == "-m" then Just (read v :: Int) else timeLimit xs

numberOfCores :: [String] -> Maybe Int
numberOfCores [] = Nothing
numberOfCores (p:v:xs) = if p == "-c" then Just (read v :: Int) else timeLimit xs

phrasesOfPower :: [String] -> [String]
phrasesOfPower [] = []
phrasesOfPower (p:v:xs) = (if p == "-p" then [v] else []) ++ phrasesOfPower xs

main :: IO ()
main = do
        args <- getArgs
        print $ args
        c <- parseFile $ head $ fileNames args
        let (s : ss) = configToStates c
        mainLoop s
    where
        mainLoop s = do
                print s
		print $ entropy1 $ board s
		let (_, s', ms) = dfs s
		print ms
		mainLoop s'
                {- m <- nextMove
                print m
                case (doMove m s) of
                        Left e -> print e
                        Right s' -> mainLoop s' -}

nextMove :: IO Move
nextMove = do
                c <- getChar
                case charToMove c of
                        Nothing -> nextMove
                        Just m -> return m
