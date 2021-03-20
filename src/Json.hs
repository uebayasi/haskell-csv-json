{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import           Control.Arrow
import           Control.Applicative ((<|>))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Foldable   (toList)
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq (..))
import qualified Data.Sequence   as Seq
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Debug.Trace     (trace)
import           Data.Vector     (Vector)
import qualified Data.Vector     as Vector
import           GHC.Generics
import           Text.Megaparsec

import Tree



conv :: [NV] -> Either String Json
conv names =
    let seq = Seq.fromList names
    in collectTree seq >>= buildJson


buildJson :: Tree -> Either String Json
buildJson (Tree (Obj obj)) =
    Json . Obj <$> mapM buildJson obj
buildJson (Tree (Ary ary)) =
    -- Fixup empty list
    -- "hoge.0,"
    -- => { "hoge": [] }
    if Map.size ary == 1 && Map.lookup 0 ary == Just (Tree (Val Text.empty)) then
        return (Json (Ary Seq.Empty))
    else
        Json . Ary . Seq.fromList <$> mapM buildJson (Map.elems ary)
buildJson (Tree (Val val)) =
    return $ Json $ Val val

    

--


newtype Json = Json (OAV (Map Text Json) (Seq Json) Text)
    deriving (Eq, Generic, Show)
instance FromJSON Json where
    parseJSON v = parseJSONObj v <|> parseJSONAry v <|> parseJSONVal v
instance ToJSON Json where
    toJSON (Json (Obj obj)) =
        object $ Map.toList $ Map.map toJSON obj
    toJSON (Json (Ary ary)) =
        Array $ Vector.fromList $ map toJSON $ toList ary
    toJSON (Json (Val txt))
        | txt == Text.empty = Null
        | otherwise = String txt


parseJSONObj :: Data.Aeson.Value -> Data.Aeson.Types.Parser Json
parseJSONObj = withObject "Json" $ \obj -> do
    let
        -- XXX Data.Foldable.toList doesn't work
        kvs :: [(Text, Value)]
        kvs = HM.toList obj
    xs <- mapM (\(k, v) -> do
            j <- parseJSON v
            return (k, j)
        ) kvs
    return $ Json $ Obj $ Map.fromList xs


parseJSONAry :: Data.Aeson.Value -> Data.Aeson.Types.Parser Json
parseJSONAry = withArray "Json" $ \ary -> do
    let
        -- XXX Data.Foldable.toList doesn't work
        vs :: [Value]
        vs = Vector.toList ary
    xs <- mapM parseJSON vs
    return $ Json $ Ary $ Seq.fromList xs


parseJSONVal :: Data.Aeson.Value -> Data.Aeson.Types.Parser Json
parseJSONVal = withText "Json" (return . Json . Val)
