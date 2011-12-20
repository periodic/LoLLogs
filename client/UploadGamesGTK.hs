module UploadGamesGTK where

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId, yield)

import Data.List (intercalate)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import UploadGamesCommon

data GUI = GUI
    { mainWin :: Window
    , logView :: TextView
    , quitBtn :: Button
    }

runUI :: [LogIO (Maybe FilePath)] -> IO ()
runUI dirOptions = do
    initGUI
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    gui <- loadGlade "Main.glade"
    connectEvents gui

    logVar <- atomically $ newTVar [] -- Holds the log.

    windowPresent $ mainWin gui

    forkIO $ logMonitor gui logVar []

    mDir <- withLog logVar $ findDir dirOptions

    case mDir of 
        Just dir -> withLog logVar $ startWorker dir >> addLogMsg "Worker started."
        Nothing  -> withLog logVar $ addLogMsg "Failed to find League of Legends log directory."

    mainGUI -- GUI loop.
    where
        startWorker dir = do
            forkLog $ uploadMain dir
        loadGlade gladepath = do
            Just xml <- xmlNew gladepath
            mw  <- xmlGetWidget xml castToWindow "windowLog"
            btn <- xmlGetWidget xml castToButton "buttonQuit"
            log <- xmlGetWidget xml castToTextView "textViewLog"
            return $ GUI mw log btn

        connectEvents gui = do
            -- main window
            onDestroy (mainWin gui) mainQuit
            onClicked (quitBtn gui) mainQuit


        logMonitor gui logVar curr = do
            let view = logView gui -- extract the text view
            logData <- atomically $ do -- Get the data, retry if it hasn't changed.
                lines <- readTVar logVar
                if length lines == length curr -- cheating and just doing length.
                    then retry
                    else return lines
            buf <- textViewGetBuffer view
            textBufferSetText buf . intercalate "\n" $ reverse logData
            logMonitor gui logVar logData

getDirFromDialog :: LogIO (Maybe FilePath)
getDirFromDialog = do
    dialog <- liftLog $ fileChooserDialogNew (Just "Locate LoL Directory") 
                                             Nothing 
                                             FileChooserActionSelectFolder 
                                             [ ("Okay", ResponseOk), ("Cancel", ResponseCancel) ]

    response <- liftLog $ dialogRun dialog

    liftLog $ widgetHide dialog
    case response of
        ResponseOk -> liftLog (fileChooserGetFilename dialog) >>= maybe (return Nothing) isValidDir
        _          -> return Nothing
