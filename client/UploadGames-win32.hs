{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, TypeFamilies, GADTs #-}
module Main where

import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)

import Control.Applicative
import Control.Exception (SomeException(..), bracket, handle)


import System.Directory (getDirectoryContents, doesFileExist)
import System.IO (hPutStrLn, stderr)

import System.Win32.Registry
import System.Win32.Types (HKEY)

import Network.URI hiding (path)
import Network.HTTP

import ParseLog

main :: IO ()
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

errorLog :: String -> IO ()
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
getLoLDirectory = getKey paths
    where 
        value = Just "LocalRootFolder"
        getKey [] = error "Unable to find LoL install directory from registry."
        getKey ((hive,path):ps) = 
            handle (\(SomeException _) -> getKey ps) $
                bracket (regOpenKey hive path) regCloseKey $ \hkey ->
                    regQueryValue hkey value

paths :: [(HKEY, String)]
paths = [ (hKEY_CURRENT_USER, "Software\\Riot Games\\RADS")
        , (hKEY_CURRENT_USER, "Software\\Classes\\VirtualStore\\MACHINE\\SOFTWARE\\Riot Games\\RADS")
        , (hKEY_CURRENT_USER, "Software\\Classes\\VirtualStore\\MACHINE\\SOFTWARE\\Wow6432Node\\Riot Games\\RADS")
        , (hKEY_CLASSES_ROOT,  "VirtualStore\\MACHINE\\SOFTWARE\\Wow6432Node\\Riot Games\\RADS")
        ]

