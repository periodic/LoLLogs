{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException(..), bracket, handle)

import System.Win32.Registry
import System.Win32.Types (HKEY)

import UploadGamesCommon
import UploadGamesGTK

main = runUI [getDirFromArgs, getDirFromRegistry]

getDirFromRegistry :: LogIO (Maybe FilePath)
getDirFromRegistry = findDir $ map getKey paths
    where 
        value = Just "LocalRootFolder"
        getKey :: (HKEY, String) -> LogIO (Maybe FilePath)
        getKey (hive,path) = liftLog $
            handle (\(SomeException _) -> return Nothing) $
                bracket (regOpenKey hive path) regCloseKey $ \hkey ->
                    Just `fmap` regQueryValue hkey value

paths :: [(HKEY, String)]
paths = [ (hKEY_CURRENT_USER, "Software\\Riot Games\\RADS")
        , (hKEY_CURRENT_USER, "Software\\Classes\\VirtualStore\\MACHINE\\SOFTWARE\\Riot Games\\RADS")
        , (hKEY_CURRENT_USER, "Software\\Classes\\VirtualStore\\MACHINE\\SOFTWARE\\Wow6432Node\\Riot Games\\RADS")
        , (hKEY_CLASSES_ROOT,  "VirtualStore\\MACHINE\\SOFTWARE\\Wow6432Node\\Riot Games\\RADS")
        ]

