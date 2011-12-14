{-# LANGUAGE OverloadedStrings #-}
module UploadGamesCommon where

import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Data.Bits

import Control.Applicative
import Control.Exception (SomeException(..), bracket, handle, Exception)
import Control.Monad
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad.Reader

import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.IO (hPutStrLn, stderr)
import System.FilePath

import Network.URI hiding (path)
import Network.HTTP (openStream, sendHTTP, getResponseBody, HandleStream, Request(..), RequestMethod(..), Header(..), HeaderName(..))

import ParseLog

import Graphics.UI.WXCore
import Graphics.UI.WX

type LogVar = (TVar [String])

data LoggerConf = LoggerConf (TVar [String])

type LogIO = ReaderT LoggerConf IO

withLog :: LogVar -> LogIO a -> IO a
withLog log = flip runReaderT (LoggerConf log)

handleLogIO :: Exception e => (e -> LogIO a) -> LogIO a -> LogIO a
handleLogIO handler task = do
    conf <- ask
    lift $ handle (flip runReaderT conf . handler) (flip runReaderT conf task)

runGUI :: [LogIO (Maybe FilePath)] -> IO ()
runGUI dirOptions = do
    tidMVar <- newEmptyMVar -- Holds tid of the actual processing thread.
    start $ initGUI tidMVar dirOptions -- Start the GUI.
    -- Check if we need to kill the othe thread.
    mTid <- tryTakeMVar tidMVar
    case mTid of
        Just tid -> killThread tid
        Nothing  -> return ()

initGUI :: MVar ThreadId -> [LogIO (Maybe FilePath)] -> IO ()
initGUI tidMVar dirOpts  = do
    -- GUI Setup
    f           <- frame        [text := "Hello!"]
    logOutput   <- staticText f [text := ""]
    quit        <- button f     [text := "Quit", on command := close f]
    set f [ layout := margin 10 (column 5 [ widget logOutput
                                          , floatCentre (widget quit)
                                          ])
          , clientSize := sz 200 200]

    -- Log stuff
    log <- atomically $ newTVar [] -- Holds the log.
    t <- timer f [interval := 100, on command := updateText log logOutput ]

    -- Get the directory.
    mDir <- withLog log $ findDir [findDir dirOpts, getDirFromDialog f]
    case mDir of
        Just dir -> do -- start the worker
            tid <- spawnWorkerThread log dir
            putMVar tidMVar tid
        Nothing -> withLog log $ addLogMsg "Unable to locate League of Legends directory."

    return ()
    where
        updateText var label = do
            log <- atomically $ readTVar var
            set label [text := intercalate "\n" . reverse . take 14 $ log]
        spawnWorkerThread :: LogVar -> FilePath -> IO ThreadId
        spawnWorkerThread log dir = forkIO $ withLog log (uploadMain dir)


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
    addLogMsg $ show respBody
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

getDirFromDialog :: Window a -> LogIO (Maybe FilePath)
getDirFromDialog win = do
    mDir <- lift $ dirOpenDialog win False "Locate LoL directory" "/"
    case mDir of
        Just dir -> isValidDir dir
        Nothing  -> addLogMsg "Unable to get directory from user." >> return Nothing

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
        then addLogMsg ("Base directory doesn't exist: " ++ path) >> return Nothing
        else handleLogIO (\(SomeException e) -> addLogMsg (show e) >> return Nothing) $ do
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
