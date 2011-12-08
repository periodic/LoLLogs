{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, TypeFamilies, GADTs #-}
module Main where

import Data.Attoparsec
import Data.Aeson 
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Maybe (catMaybes)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)

import System.Environment (getArgs)
import System.FilePath (takeFileName, replaceExtension)
import System.IO (hPutStrLn, stderr)

import Network.HTTP.Enumerator
import Network.HTTP.Types

import ParseLog
import Data.GameLog

{- The steps:
 -
 - 1. Raw ByteString -> Parser ASValue  \___ gamesAsJSON
 - 2. ASValue -> JSON ByteString        / 
 - 3. JSON ByteString -> Parser Aeson.Result GameStats
 - 4. GameStats -> CSV ByteString
 -}

main = do
    [dir] <- getArgs
    isDir <- doesDirectoryExist dir
    files <- if isDir
             then map (dir ++) <$> getDirectoryContents dir
             else return [dir]
    games <- mapM processFile files
    manager <- newManager
    mapM_ (uploadGame manager undefined) . catMaybes $ games

processFile :: FilePath -> IO (Maybe BS.ByteString)
processFile path = do
    errorLog $ "Parsing file " ++ path
    exists <- doesFileExist path
    if exists
        then (Just . gamesAsJSON) <$> BS.readFile path
        else errorLog "File does not exist." >> return Nothing

errorLog = hPutStrLn stderr


uploadGame :: Manager -> String -> BS.ByteString -> IO ()
uploadGame manager url jsonGames = do
    let request = def { method = methodPost
                      , host   = "localhost"
                      , port   = 3000
                      , path   = "/game"
                      , requestBody = RequestBodyBS jsonGames
                      }
    resp <- httpLbsRedirect request manager
    case resp of
        Response code _ str -> putStr (show code ++ ": ") >> mapM_ BS.putStrLn (L.toChunks str)

