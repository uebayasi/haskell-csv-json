{-# LANGUAGE OverloadedStrings #-}

module TestData where

import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq(..))
import qualified Data.Sequence   as Seq
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Megaparsec

import Json
import Tree



-- "group.0.name,野菜"
--   =>


csv1 :: Json
csv1 =
    Json $ Obj $ Map.fromList
        [ ( "group",
            Json $ Ary $ Seq.fromList
                [ Json $ Obj $ Map.fromList
                    [ ( "name",
                        Json (Val "野菜")
                      )
                    ]
                ]
          )
        ]


row1 :: Name
row1 = Name (Obj ("group", Name (Ary (0, Name (Obj ("name", Name (Val ())))))))
row2 :: Name
row2 = Name (Obj ("title", Name (Val ())))
row3 :: Name
row3 = Name (Obj ("date", Name (Val ())))
row4 :: Name
row4 = Name (Obj ("file", Name (Ary (0, Name (Val ())))))
row5 :: Name
row5 = Name (Obj ("file", Name (Ary (1, Name (Val ())))))


testRows :: [(Name, Text)]
testRows =
    [ (row1, "野菜")
    , (row2, "test data")
    , (row3, "2021-01-01")
    , (row4, "data.csv")
    , (row5, "logo.svg")
    ]



{-

group.0.name,野菜
group.1.name,果物
title,test data
date,2021-01-01
file.0.config.yaml
file.1.main.csv
file.2.logo.svg

-}
