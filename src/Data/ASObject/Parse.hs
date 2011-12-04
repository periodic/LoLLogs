{-# LANGUAGE OverloadedStrings, FlexibleInstances, OverlappingInstances #-}
module Data.ASObject.Parse ( parseActionScript
                           , FromAS(..)
                           , ToAS(..)
                           , (.:)
                           ) where

-- Parser
import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

-- Bytestrings
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

-- Data types
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Attoparsec.Number as N

-- Control flow
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Reader

-- ASValue
import Data.ASObject.Types

type ValueParser = ReaderT Int Parser

class FromAS a where
    parseAS :: ASValue -> Parser a

class ToAS a where
    toAS :: a -> ASValue

instance FromAS ASValue where
    parseAS value = return value

instance ToAS ASValue where
    toAS value = value

instance ToAS Integer where
    toAS = ASNumber . N.I
instance ToAS BS.ByteString where
    toAS = ASString . TE.decodeUtf8
instance ToAS Bool where
    toAS = ASBoolean
instance ToAS a => ToAS (Maybe a) where
    toAS = maybe ASNull toAS
instance ToAS a => ToAS [a] where
    toAS = ASArray (-1) . V.fromList . map toAS
instance ToAS [Char] where
    toAS = ASString . T.pack
instance ToAS N.Number where
    toAS = ASNumber

(.:) :: (FromAS a) => Object -> T.Text -> Parser a
obj .: key = case M.lookup key obj of
                Nothing -> fail $ "key " ++ show key ++ " not present"
                Just v  -> parseAS v

value :: ValueParser ASValue
value = nullVal <|> nanVal <|> booleanVal <|> numVal <|> dateVal <|> stringVal <|> objectVal <|> arrayVal <|> mapVal
    where
        numVal     = toAS <$> number
        nanVal     = ASNumber (N.D (0/0)) <$ literal "NaN"
        booleanVal = (toAS False <$ literal "false") <|> (toAS True <$ literal "true")
        stringVal  = toAS <$> (quote *> takeWhileL (not . inClass "\"") <* quote)
        dateVal    = ASDate . TE.decodeUtf8 . BS.concat <$> sequence [(foldr1 (<|>) . map literal $ ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]), lift $ takeTill P8.isEndOfLine]
        nullVal    = ASNull <$ literal "(null)"
        objectVal  = ASObject <$> objectIdentifier <* char '#' <*> integer <* whiteSpace <*> (M.fromList <$> indentedMany pair)
        mapVal     = ASObject T.empty <$> (literal "(Object)#" *> integer) <* whiteSpace <*> (M.fromList <$> indentedMany pair)
        arrayVal   = ASArray <$> (literal "(Array)#" *> integer) <* whiteSpace <*> (V.fromList . map snd <$> indentedMany arrayPair)

        pair = (,) <$> (TE.decodeUtf8 <$> identifier) <* whiteSpace <* char '=' <* whiteSpace <*> value
        arrayPair = (,) <$> (char '[' *> integer) <* char ']' <* whiteSpace <*> value

char = lift . char8
quote = lift . satisfy $ inClass "\""
openParen = lift . satisfy $ inClass "("
closeParen = lift . satisfy $ inClass ")"
space = lift . satisfy $ inClass " "
whiteSpace = many space
endOfLineL = lift endOfLine
number = lift P8.number
integer = lift P8.decimal
literal = lift . string
takeWhileL = lift . P.takeWhile
identifier = lift . takeWhile1 $ inClass "A-Za-z0-9_-"
objectIdentifier = do
    openParen
    package <- takeWhileL (inClass "a-zA-Z.")
    literal "::"
    objectClass <- takeWhileL (inClass "a-zA-Z0-9")
    closeParen
    return . TE.decodeUtf8 . BS.concat $ [package, "::", objectClass]
indent = do
    spaces <- ask
    sequence . replicate spaces $ space
withIndent = local (+2)
indentedMany p = (withIndent $ many (endOfLineL *> indent *> p))


runParserOnly parser input = parseOnly (runReaderT parser 0) input
runParser     parser input = parse     (runReaderT parser 0) input

parseActionScript = runReaderT value 0

