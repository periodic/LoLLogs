{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module ParseLog where

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Reader
import Data.Maybe (catMaybes)

import Data.ASObject
import Data.Aeson (encode)

data Log = Log { logLines :: [LogLine] } deriving Show

parseLog = Log <$> many (logLine <* many (satisfy $ inClass "\n \r"))

data LogLine = LogLine { logTimestamp :: S.ByteString
                       , logLevel     :: S.ByteString
                       , logMessage   :: Message
                       } deriving (Show)
whiteSpace = P.takeWhile (inClass " ")

logLine = LogLine <$> dateTime <* whiteSpace <*> level <* whiteSpace <*> message

dateTime :: Parser S.ByteString
dateTime = S.concat <$> sequence [digits, string "/", digits, string "/", digits, string " ", digits, string ":", digits, string ":", digits, string ".", digits]
    where
        digits = P.takeWhile1 $ inClass "0-9"

level  =  string "["
       *> (foldr1 (<|>) . map string $ ["INFO", "DEBUG", "WARN", "ERROR", "FATAL"])
      <*  string "]"

message = endOfGameStats <|> sentMessage <|> other
    where
        endOfGameStats = do
            string "com.riotgames.platform.gameclient.module.services.RemoteObjectGenerator Got async message:"
            whiteSpace
            obj@(ASObject _ _ properties) <- parseActionScript
            case M.lookup "body" properties of
                Just stats@(ASObject name _ _) ->
                    if (name == "com.riotgames.platform.gameclient.domain::EndOfGameStats")
                    then return . EndOfGameStats $ stats
                    else return . AsyncMessageExt $ obj
                _ -> return . AsyncMessageExt $ obj
        sentMessage = do
            string "com.riotgames.platform.gameclient.module.services.RemoteObjectGenerator Sending message:"
            whiteSpace
            SentMessage <$> parseActionScript
        other = Other <$> P.takeTill (inClass "\n")

data Message = AsyncMessageExt ASValue
             | EndOfGameStats ASValue
             | SentMessage ASValue
             | Other S.ByteString
             deriving(Show)

gamesAsJSON :: S.ByteString -> L.ByteString
gamesAsJSON logText = encode gameStats
    where
        lines = parseOnly parseLog logText
        gameStats = case lines of
            Left err          -> [toAS err]
            Right (Log lines) -> catMaybes . map maybeStats $ lines
        maybeStats l = case logMessage l of 
            (EndOfGameStats stats) -> Just stats
            _                      -> Nothing
