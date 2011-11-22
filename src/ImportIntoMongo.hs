{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, TypeFamilies, GADTs #-}
module Main where

import Data.Attoparsec
import Data.Aeson 
import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.ByteString as BS

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)

import System.Environment (getArgs)
import System.FilePath (takeFileName, replaceExtension)
import System.IO (hPutStrLn, stderr)

import Language.Haskell.TH.Syntax (Type(..))

import Database.Persist
import Database.Persist.Base
import Database.Persist.MongoDB
import Database.Persist.TH

import ParseLog
import Data.GameLog

-- Persist stuff.
mkPersist MkPersistSettings { mpsBackend = ConT ''Action } [persist|
Game
    created UTCTime
    gameStats GameStats
|]

main = do
    [dir] <- getArgs
    isDir <- doesDirectoryExist dir
    files <- if isDir
             then map (dir ++) <$> getDirectoryContents dir
             else return [dir]
    games <- concat <$> mapM processFile files
    time <- getCurrentTime
    insertGames time games

{- The steps:
 -
 - 1. Raw ByteString -> Parser ASValue  \___ gamesAsJSON
 - 2. ASValue -> JSON ByteString        / 
 - 3. JSON ByteString -> Parser Aeson.Result GameStats
 - 4. GameStats -> CSV ByteString
 -}

processFile :: FilePath -> IO [GameStats]
processFile path = do
    errorLog $ "Parsing file " ++ path
    exists <- doesFileExist path
    if exists
        then do
            parsed <- parseOnly (fromJSON <$> json) . gamesAsJSON <$> BS.readFile path
            case parsed of 
                Right (Success games) -> do
                    errorLog $ "Found " ++ show (length games) ++ " games."
                    return games
        else errorLog "File does not exist." >> return []

errorLog = hPutStrLn stderr

insertGames :: UTCTime -> [GameStats] -> IO ()
insertGames time games = withMongoDBConn "LoLLogsWebApp" "localhost" $ runMongoDBConn master $ do
    time <- liftIO getCurrentTime
    mapM_ (insert . Game time) games

