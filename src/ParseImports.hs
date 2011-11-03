{-# LANGUAGE OverloadedStrings #-}
module ParseImports where

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8

import Control.Applicative


word = P.takeWhile (inClass "a-zA-Z-")
mimeType = word
mimeSubType = word

accept = do
    string "Accept: "
    some ((,) <$> mediaRange <*> acceptParams)

media-range = (,) 
    <$> string "*/*" 
        <|> (S.concat <$> sequence [mimeType, string "/*"])
        <|> (S.concat <$> sequence [mimeType, string "/", mimeSubType])
    <*> many (string ";" *> parameter)

acceptParams = string ";q=" *> (,) <$> P8.double <*> many acceptExtension

acceptExtension = return ""
