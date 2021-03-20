{-# LANGUAGE OverloadedStrings #-}

module Parse where

import           Control.Applicative ((<|>))
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Tree



type Parser a = Parsec String Text a


parseName :: Text -> Either String Name
parseName k =
    case parse parseName1 "" k of
        Left e -> Left (show e)
        Right cj ->
            return cj


parseName1 :: Parser Name
parseName1 =
    parseArray <|> parseObject


parseArray :: Parser Name
parseArray = do
    idx <- integer
    let
        term :: Parser Name
        term = do
            eof
            return $ Name (Ary (idx, Name (Val ())))

        more :: Parser Name
        more = do
            dot
            rest <- parseName1
            return $ Name (Ary (idx, rest))
    term <|> more


parseObject :: Parser Name
parseObject = do
    n <- name
    let
        term :: Parser Name
        term = do
            eof
            return $ Name (Obj (n, Name (Val ())))

        more :: Parser Name
        more = do
            dot
            rest <- parseName1
            return $ Name (Obj (n, rest))
    term <|> more


name :: Parser Text
name = do
    a <- anySingleBut '.'
    b <- many (anySingleBut '.')
    return $ Text.pack $ a:b


--


lexeme = L.lexeme spaceConsumer


spaceConsumer :: Parser ()
spaceConsumer = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")


integer :: Parser Int
integer = lexeme L.decimal


dot :: Parser ()
dot = do
    L.symbol spaceConsumer "."
    return ()
