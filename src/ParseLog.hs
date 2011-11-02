{-# LANGUAGE OverloadedStrings #-}
module ParseLog where

import Prelude hiding (takeWhile)
import Data.Attoparsec.Char8
import qualified Data.ByteString as S
import Control.Applicative hiding (many)

data Log = Log deriving Show

{-
parseFile :: FilePath -> IO ()
parseFile path = parseOnly logParser <$> S.readFile path >>= print
-}

data Value = NumberValue Integer
           | StringValue S.ByteString
           | BooleanValue Bool
           | NullValue
           | ObjectValue S.ByteString [(S.ByteString, Value)]
           deriving (Show, Eq)

value :: Parser Value
value = null <|> boolean <|> str <|> number
    where
        number  = NumberValue . read <$> many digit 
        boolean = (string "false" *> return (BooleanValue False)) <|> (string "true" *> return (BooleanValue True))
        str     = StringValue <$> (char '"' *> takeWhile (not . (== '"')) <* char '"')
        null    = string "(null)" *> return NullValue

object = do
    char '('
    name <- objectType
    endOfLine
    pairs <- many (pair <* endOfLine)
    return $ ObjectValue name pairs

pair = do
    k <- word
    whiteSpace
    char '='
    whiteSpace
    v <- value
    return (k, v)

objectType = char '(' *> takeWhile (not . (== ')')) <* char ')'

whiteSpace = many space

word = takeWhile1 (not . isSpace)

indent spaces = sequence . replicate spaces $ space

withIndent spaces parser = indent spaces *> parser
