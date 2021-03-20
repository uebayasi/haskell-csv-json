module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import System.Environment
import System.IO

import qualified Csv
import qualified Json
import qualified Parse
import Tree (NV(NV))



main :: IO ()
main = do
    args <- getArgs
    case args of
        (csvFile:jsonFile:_) -> do
            h <- openFile csvFile ReadMode 
            contents <- BSL.hGetContents h
            case Csv.decode contents of
                Left e ->
                    fail e
                Right xs -> do
                    let
                        json = either undefined id $ do
                            cj <- conv xs
                            return (toJSON cj)
                    encodeFile jsonFile json


conv :: [(Text, Text)] -> Either String Json.Json
conv xs = do
    nvs <- mapM (\(n, v) -> (`NV` v) <$> Parse.parseName n) xs
    Json.conv nvs
