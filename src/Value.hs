module Value ( Value(..)
             , parseValue
             ) where

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as S
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Reader

data Value = NumberValue Integer
           | NaNValue
           | StringValue S.ByteString
           | BooleanValue Bool
           | NullValue
           | DateValue S.ByteString
           | ArrayValue Integer [Value]
           | ObjectValue S.ByteString Integer [(S.ByteString, Value)]
           | MapValue Integer [(S.ByteString, Value)]
           deriving (Show, Eq)

{-
newtype ValueParser a = VP {
    runValueParser :: ReaderT Int Parser a
    } deriving (Monad, Functor, Applicative, Alternative, MonadReader Int)
    -}
type ValueParser = ReaderT Int Parser

value :: ValueParser Value
value = nullVal <|> nanVal <|> booleanVal <|> numVal <|> dateVal <|> stringVal <|> objectVal <|> arrayVal <|> mapVal
    where
        numVal     = NumberValue <$> number
        nanVal     = NaNValue <$ literal "NaN"
        booleanVal = (BooleanValue False <$ literal "false") <|> (BooleanValue True <$ literal "true")
        stringVal  = StringValue <$> (quote *> takeWhileL (not . inClass "\"") <* quote)
        dateVal    = DateValue . S.concat <$> sequence [(foldr1 (<|>) . map literal $ ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]), lift $ takeTill P8.isEndOfLine]
        nullVal    = NullValue <$ literal "(null)"
        objectVal  = ObjectValue <$> objectIdentifier <* char '#' <*> number <* whiteSpace <*> indentedMany pair
        mapVal     = MapValue <$> (literal "(Object)#" *> number) <* whiteSpace <*> indentedMany pair
        arrayVal   = ArrayValue <$> (literal "(Array)#" *> number) <* whiteSpace <*> (map snd <$> indentedMany arrayPair)

        pair = (,) <$> identifier <* whiteSpace <* char '=' <* whiteSpace <*> value
        arrayPair = (,) <$> (char '[' *> number) <* char ']' <* whiteSpace <*> value

char = lift . char8
quote = lift . satisfy $ inClass "\""
openParen = lift . satisfy $ inClass "("
closeParen = lift . satisfy $ inClass ")"
space = lift . satisfy $ inClass " "
whiteSpace = many space
endOfLineL = lift endOfLine
number = lift $ P8.signed P8.decimal
literal = lift . string

takeWhileL = lift . P.takeWhile

identifier = lift . takeWhile1 $ inClass "A-Za-z0-9_-"

objectIdentifier = do
    openParen 
    package <- takeWhileL (inClass "a-zA-Z.") 
    literal "::"
    objectClass <- takeWhileL (inClass "a-zA-Z0-9") 
    closeParen
    return . S.concat $ [package, "::", objectClass]

indent = do
    spaces <- ask 
    sequence . replicate spaces $ space

withIndent = local (+2)
indentedMany p = (withIndent $ many (endOfLineL *> indent *> p))

runValueParserOnly parser input = parseOnly (runReaderT parser 0) input
runValueParser     parser input = parse     (runReaderT parser 0) input

parseValue = runReaderT value 0


