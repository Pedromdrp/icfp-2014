import Datastructures
import Parsing
import LoadConfig

main :: IO ()
main = do
        c <- parseFile "qualifiers/problem_24.json"
        let (s : ss) = configToStates c
        print (board s)
