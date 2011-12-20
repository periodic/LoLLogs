module UploadGamesWX where

import Graphics.UI.WXCore
import Graphics.UI.WX

import Control.Applicative
import Control.Exception (SomeException(..), bracket, handle, Exception)
import Control.Monad
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.MVar

import UploadGamesCommon

runUI :: [LogIO (Maybe FilePath)] -> IO ()
runUI dirOptions = do
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

getDirFromDialog :: Window a -> LogIO (Maybe FilePath)
getDirFromDialog win = do
    mDir <- lift $ dirOpenDialog win False "Locate LoL directory" "/"
    case mDir of
        Just dir -> isValidDir dir
        Nothing  -> addLogMsg "Unable to get directory from user." >> return Nothing

