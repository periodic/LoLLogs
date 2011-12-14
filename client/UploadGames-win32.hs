{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)

import Control.Applicative
import Control.Exception (SomeException(..), bracket, handle)

import System.IO (hPutStrLn, stderr)

import System.Win32.Registry
import System.Win32.Types (HKEY)

import Network.URI hiding (path)
import Network.HTTP

import ParseLog
import GuiLog
import UploadGamesCommon

main = uploadMain [getDirFromArgs, getDirFromRegistry]

getDirFromRegistry :: IO (Maybe FilePath)
getDirFromRegistry = findDir $ map getKey paths
    where 
        value = Just "LocalRootFolder"
        getKey [] = return Nothing
        getKey (hive,path) = 
            handle (\(SomeException _) -> return Nothing) $
                bracket (regOpenKey hive path) regCloseKey $ \hkey ->
                    regQueryValue hkey value

paths :: [(HKEY, String)]
paths = [ (hKEY_CURRENT_USER, "Software\\Riot Games\\RADS")
        , (hKEY_CURRENT_USER, "Software\\Classes\\VirtualStore\\MACHINE\\SOFTWARE\\Riot Games\\RADS")
        , (hKEY_CURRENT_USER, "Software\\Classes\\VirtualStore\\MACHINE\\SOFTWARE\\Wow6432Node\\Riot Games\\RADS")
        , (hKEY_CLASSES_ROOT,  "VirtualStore\\MACHINE\\SOFTWARE\\Wow6432Node\\Riot Games\\RADS")
        ]

