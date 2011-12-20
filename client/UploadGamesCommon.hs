{-# LANGUAGE OverloadedStrings #-}
module UploadGamesCommon where

import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Bits
import Data.String.UTF8 as UTF8 (toString, fromRep)

import Control.Applicative
import Control.Exception (SomeException(..), bracket, handle, Exception)
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Concurrent (ThreadId, forkIO)

import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath

import Network.URI hiding (path)
import Network.HTTP (openStream, sendHTTP, getResponseBody, HandleStream, Request(..), RequestMethod(..), Header(..), HeaderName(..))

import ParseLog

type LogVar = (TVar [String])

data LoggerConf = LoggerConf (TVar [String])

type LogIO = ReaderT LoggerConf IO

withLog :: LogVar -> LogIO a -> IO a
withLog log = flip runReaderT (LoggerConf log)

handleLogIO :: Exception e => (e -> LogIO a) -> LogIO a -> LogIO a
handleLogIO handler task = do
    conf <- ask
    lift $ handle (flip runReaderT conf . handler) (flip runReaderT conf task)

liftLog :: IO a -> LogIO a
liftLog = lift

forkLog :: LogIO () -> LogIO ThreadId
forkLog action = do
    conf <- ask
    lift . forkIO . (flip runReaderT conf) $ action

getLogData :: LogIO [String]
getLogData = do
    (LoggerConf var) <- ask
    lift $ atomically (reverse <$> readTVar var)

addLogMsg :: String -> LogIO ()
addLogMsg msg = do
    (LoggerConf log) <- ask
    lift . atomically $ do
        lines <- readTVar log
        writeTVar log (msg : lines)

uploadMain :: FilePath -> LogIO ()
uploadMain path = do
    files <- getLogFilePaths path
    games <- mapM (processFile) files
    conn <- lift $ openStream "lol.casualaddict.com" 80
    mapM_ (uploadGame conn) . catMaybes $ games
    addLogMsg "Done"

processFile :: FilePath -> LogIO (Maybe BS.ByteString)
processFile path = do
    addLogMsg $ "Parsing file " ++ path
    exists <- lift $ doesFileExist path
    if exists
        then (Just . gamesAsJSON) <$> lift (BS.readFile path)
        else addLogMsg "File does not exist." >> return Nothing

uploadGame :: HandleStream BS.ByteString -> BS.ByteString -> LogIO ()
uploadGame conn jsonGames = do
    rawResponse <- lift $ sendHTTP conn request
    respBody <- lift $ getResponseBody rawResponse
    addLogMsg . UTF8.toString . UTF8.fromRep $ respBody
    where
        uri = maybe undefined id $ parseURI "http://lol.casualaddict.com/game"
        request = Request { rqURI = uri
                          , rqMethod = POST
                          , rqHeaders = [ Header HdrContentType "application/json; charset=utf-8"
                                        , Header HdrContentLength (show $ BS.length jsonGames)
                                        ]
                          , rqBody = jsonGames
                          }

{- | Take a list of actions that check file paths and return maybe a file path.
 - Then return the first one that is not Nothing.
 -}
findDir :: [LogIO (Maybe FilePath)] -> LogIO (Maybe FilePath)
findDir [] = return Nothing
findDir (mpAct:mpActs) = do
    mp <- mpAct
    case mp of
        Just p  -> return mp
        Nothing -> findDir mpActs

getDirFromArgs :: LogIO (Maybe FilePath)
getDirFromArgs = do
    args <- lift getArgs
    case args of
        [] -> addLogMsg "No directory in arguments." >> return Nothing
        _  -> findDir $ map (isValidDir) args

getLogFileDirs :: FilePath -> LogIO [FilePath]
getLogFileDirs lolDir = do
    let versionDir = makePath [lolDir, "RADS", "projects", "lol_air_client", "releases"]
    versions <- lift $ getDirectoryContentsWithPrefix versionDir
    return $ map (\p -> makePath [p, "deploy", "logs"]) versions

getLogFilePaths :: FilePath -> LogIO [FilePath]
getLogFilePaths lolDir = do
    logDirs <- getLogFileDirs lolDir
    concat <$> mapM (lift . getDirectoryContentsWithPrefix) logDirs

isValidDir :: FilePath -> LogIO (Maybe FilePath)
isValidDir path = do
    addLogMsg $ "Checking directory: " ++ path
    baseExists <- lift $ doesDirectoryExist path
    if not baseExists
        then addLogMsg ("Directory doesn't exist: " ++ path) >> return Nothing
        else handleLogIO (\(SomeException e) -> addLogMsg "Directory does not exist or is not a League of Legends install directory." >> return Nothing) $ do
            logDirs <- getLogFileDirs path
            addLogMsg $ show logDirs
            logDirsExist <- mapM (lift . doesDirectoryExist) logDirs
            if or logDirsExist
                then return $ Just path
                else addLogMsg ("No valid log subdirectories exist.") >> return Nothing

getDirectoryContentsWithPrefix :: FilePath -> IO [FilePath]
getDirectoryContentsWithPrefix path =
    map ((path ++ [pathSeparator]) ++ ) . filter (not . flip elem [".", ".."])
    <$> getDirectoryContents path

makePath = intercalate [pathSeparator]
