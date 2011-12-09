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
import Control.Exception (bracket)


import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (takeFileName, replaceExtension)
import System.IO (hPutStrLn, stderr)

import System.Win32.Registry

import Network.URI
import Network.HTTP
import Network.TCP


import ParseLog
import Data.GameLog

main = do
    files <- getLogFilePaths
    games <- mapM processFile files
    conn <- openStream "lol.casualaddict.com" 80
    mapM_ (uploadGame conn) . catMaybes $ games

processFile :: FilePath -> IO (Maybe BS.ByteString)
processFile path = do
    errorLog $ "Parsing file " ++ path
    exists <- doesFileExist path
    if exists
        then (Just . gamesAsJSON) <$> BS.readFile path
        else errorLog "File does not exist." >> return Nothing

errorLog = hPutStrLn stderr


uploadGame :: HandleStream BS.ByteString -> BS.ByteString -> IO ()
uploadGame conn jsonGames = do
    rawResponse <- sendHTTP conn request
    respBody <- getResponseBody rawResponse
    BS.putStrLn respBody
    where
        uri = maybe undefined id $ parseURI "http://lol.casualaddict.com/game"
	request = Request { rqURI = uri
                          , rqMethod = POST
                          , rqHeaders = [ Header HdrContentType "application/json; charset=utf-8"
                                        , Header HdrContentLength (show $ BS.length jsonGames)
                                        ]
                          , rqBody = jsonGames
                          }

getLogFilePaths :: IO [FilePath]
getLogFilePaths = do
    lolDir <- getLoLDirectory
    let versionDir = lolDir ++ "\\projects\\lol_air_client\\releases"
    versions <- getDirectoryContentsWithPrefix versionDir
    concat <$> mapM (getDirectoryContentsWithPrefix . (++ "\\deploy\\logs")) versions

getDirectoryContentsWithPrefix :: FilePath -> IO [FilePath]
getDirectoryContentsWithPrefix path = 
    map ((path ++ "\\") ++ ) . filter (not . flip elem [".", ".."]) 
    <$> getDirectoryContents path

getLoLDirectory :: IO FilePath
getLoLDirectory = bracket (regOpenKey hive path) regCloseKey $ \hkey ->
        regQueryValue hkey (Just "LocalRootFolder")
    where
        hive = hKEY_CURRENT_USER
        path = "Software\\Riot Games\\RADS"


