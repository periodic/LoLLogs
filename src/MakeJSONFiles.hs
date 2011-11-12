module Main where

import System.IO (hPutStrLn, stderr)
import Data.Maybe (catMaybes)
import Data.Either
import Data.Attoparsec
import Data.Aeson 

import qualified Data.ByteString as BS
import ParseLog
import Control.Applicative
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import System.FilePath (takeFileName, replaceExtension)

main = do
    [dir] <- getArgs
    isDir <- doesDirectoryExist dir
    files <- if isDir
             then map (dir ++) <$> getDirectoryContents dir
             else return [dir]
    mapM_ processFile $ files

processFile :: FilePath -> IO ()
processFile path = do
    putStrLn $ "Parsing file " ++ path
    exists <- doesFileExist path
    if exists
        then BS.readFile path >>= BS.writeFile ((flip replaceExtension) "json" . takeFileName $ path) . gamesAsJSON
        else errorLog "File does not exist." >> return ()
    where

errorLog = hPutStrLn stderr
