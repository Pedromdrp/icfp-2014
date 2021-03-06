{-# LANGUAGE ScopedTypeVariables #-}
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
import Targetting
import Control.Monad
import Debug.Trace

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
          print $ entropy $ board s
          let mm :: Maybe [Move] = (msum . map return) $ do
                (tgt,lm) <- (transforms3 s)
                case findMove s tgt of
                        Just ms -> return (ms ++ [lm])
                        Nothing -> mzero
          print mm
          case mm of
                Nothing -> do
                         m <- nextMove
                         print m
                         case (doMove m s) of
                           Left e -> print e
                           Right s' -> mainLoop s'
                Just moves -> case applyMoves moves s of
                        Left e -> print e
                        Right s' -> mainLoop s'


applyMoves :: [Move] -> State -> Either GameOver State
applyMoves [] s = Right s
applyMoves (x:xs) s =
  case newState of
    Left e -> newState
    Right s' -> applyMoves xs s'
  where newState = doMove x s

nextMove :: IO Move
nextMove = do
                c <- getChar
                case charToMove c of
                        Nothing -> nextMove
                        Just m -> return m
