{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import Datastructures
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B

instance FromJSON Cell where
 parseJSON (Object v) =
    Cell <$> v .: "x"
         <*> v .: "y"
 parseJSON _ = mzero

instance FromJSON Unit where
 parseJSON (Object v) =
    Unit <$> v .: "members"
         <*> v .: "pivot"
 parseJSON _ = mzero

instance FromJSON Configuration where
 parseJSON (Object v) =
    Configuration <$> v .: "id"
                  <*> v .: "units"
                  <*> v .: "width"
                  <*> v .: "height"
                  <*> v .: "filled"
                  <*> v .: "sourceLength"
                  <*> v .: "sourceSeeds"
 parseJSON _ = mzero


parseFile :: FilePath -> IO Configuration
parseFile file = do
        d <- (eitherDecode <$> B.readFile file) :: IO (Either String Configuration)
        case d of
                Left err -> error err
                Right ps -> return ps

{- 
jsonFile :: FilePath
jsonFile = "qualifiers/problem_0.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String Configuration)
  case d of
    Left err -> putStrLn err
    Right ps -> print ps
-}
