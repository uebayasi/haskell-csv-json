{-# LANGUAGE DeriveGeneric #-}

module Csv where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import           GHC.Generics



decode :: BSL.ByteString -> Either String [(Text, Text)]
decode bs =
    case Data.Csv.decodeByName bs of
        Left err ->
            Left err
        Right (h, v) ->
            Right ((map toTuple . Vector.toList) v)


data NameValue = NameValue
    { name :: Text
    , value :: Text
    }
    deriving (Generic, Show)
instance Data.Csv.FromNamedRecord NameValue
instance Data.Csv.ToNamedRecord NameValue


toTuple (Csv.NameValue n v) = (n, v)
