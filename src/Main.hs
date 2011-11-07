module Main where

import LoLStats
import Game.Log
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (catMaybes)
import qualified Data.ByteString as BS
import ParseLog
import Control.Applicative
import Data.Attoparsec
import Data.Aeson 

main = do
    [dir] <- getArgs
    files <- getDirectoryContents dir
    putStrLn csvHeader
    let printCSV file = processFile (dir ++ file) >>= mapM_ (putStrLn . toCSV)
    mapM_ printCSV $ files


processFile :: FilePath -> IO [StatsRow]
processFile path = do
    exists <- doesFileExist path
    if exists
        then do
            parsed <- parseOnly (fromJSON <$> json) . gamesAsJSON <$> BS.readFile path
            case parsed of 
                Right (Success rows) -> return . catMaybes . map getStats $ rows
                Right (Error   msg ) -> putStrLn ("JSON error: " ++ msg) >> return []
                Left msg             -> putStrLn msg >> return []
        else return []
