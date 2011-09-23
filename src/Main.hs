module Main where

import LoLStats
import LoLLogs
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (catMaybes)

main = do
    [dir] <- getArgs
    files <- getDirectoryContents dir
    putStrLn csvHeader
    let printCSV file = processFile (dir ++ file) >>= maybe (return ()) (putStrLn . toCSV)
    sequence . map printCSV $ files


processFile :: FilePath -> IO (Maybe StatsRow)
processFile path = do
    exists <- doesFileExist path
    if exists
        then do
            eGame <- parseFile path
            case eGame of
                Left game -> return . getStats $ game
                Right err -> hPutStrLn stderr (path ++ " :: " ++ err) >> return Nothing
        else return Nothing
