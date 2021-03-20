{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Tree where

import           Control.Applicative ((<|>))
import           Control.Monad   (foldM)
import           Data.Foldable   (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Sequence   (Seq (..))
import qualified Data.Sequence   as Seq
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Debug.Trace     (trace)
import           GHC.Generics



data OAV o a v = Obj o | Ary a | Val v
    deriving (Eq, Show)


data NV = NV Name Text
    deriving (Eq, Show)


newtype Name = Name (OAV (Text, Name) (Int, Name) ())
    deriving (Eq, Show)


newtype Tree = Tree (OAV (Map Text Tree) (Map Int Tree) Text)
    deriving (Eq, Show)



--


collectTree :: Seq NV -> Either String Tree
collectTree =
    maybe (Left "collectTree") Right . collectTree1


collectTree1 :: Seq NV -> Maybe Tree
collectTree1 nvs =
    (Tree . Obj <$> (foldM obj Map.empty nvs >>= mapM collectTree1)) <|>
    (Tree . Ary <$> (foldM ary Map.empty nvs >>= mapM collectTree1)) <|>
    (Tree . Val <$> foldM val Text.empty nvs)


type Col a = Map a (Seq NV)


obj :: Col Text -> NV -> Maybe (Col Text)
obj o (NV (Name (Obj (name, rest))) val) = Just $ Map.alter (g (NV rest val)) name o
obj _ _ = Nothing


ary :: Col Int -> NV -> Maybe (Col Int)
ary a (NV (Name (Ary (idx, rest))) val) = Just $ Map.alter (g (NV rest val)) idx a
ary _ _ = Nothing


val :: Text -> NV -> Maybe Text
val t (NV (Name (Val _)) val)
    | t == Text.empty = Just val
    | otherwise = Nothing
val _ _ = Nothing


g :: NV -> Maybe (Seq NV) -> Maybe (Seq NV)
g nv = Just . (:|> nv) . fromMaybe Seq.Empty
