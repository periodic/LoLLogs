module Main where

import LoLStats
import LoLLogs
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import Data.Maybe (catMaybes)

main = do
    files <- getCurrentDirectory >>= getDirectoryContents
    stats <- sequence . map processFile $ files
    putStrLn csvHeader
    sequence . map (putStrLn . toCSV) . catMaybes $ stats


processFile :: FilePath -> IO (Maybe StatsRow)
processFile path = do
    exists <- doesFileExist path
    if exists
        then do
            eGame <- parseFile path
            case eGame of
                Left game -> return . getStats $ game
                Right _   -> return Nothing
        else return Nothing
