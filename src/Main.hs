module Main where

import LoLStats
import Game.Log
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (catMaybes)
import Data.Either
import qualified Data.ByteString as BS
import ParseLog
import Control.Applicative
import Data.Attoparsec
import Data.Aeson 

main = do
    [dir] <- getArgs
    isDir <- doesDirectoryExist dir
    files <- if isDir
             then map (dir ++) <$> getDirectoryContents dir
             else return [dir]
    putStrLn csvHeader
    let printCSV file = processFile file >>= mapM_ (putStrLn . toCSV)
    mapM_ printCSV $ files

{- The steps:
 -
 - 1. Raw ByteString -> Parser ASValue  \___ gamesAsJSON
 - 2. ASValue -> JSON ByteString        / 
 - 3. JSON ByteString -> Parser Aeson.Result GameStats
 - 4. GameStats -> CSV ByteString
 -}

processFile :: FilePath -> IO [StatsRow]
processFile path = do
    putStrLn $ "Parsing file " ++ path
    exists <- doesFileExist path
    if exists
        then do
            parsed <- parseOnly (fromJSON <$> json) . gamesAsJSON <$> BS.readFile path
            case parsed of 
                Right (Success rows) -> do
                    putStrLn ("Found " ++ show (length rows) ++ " games.") 
                    let stats = map (getStats "ShaperOfChaos") rows
                        successes = rights stats
                        errors = lefts stats
                    mapM_ putStrLn errors
                    return successes
                Right (Error   msg ) -> putStrLn ("JSON error: " ++ msg) >> return []
                Left msg             -> putStrLn msg >> return []
        else putStrLn "File does not exist." >> return []
