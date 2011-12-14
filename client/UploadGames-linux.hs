{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, TypeFamilies, GADTs #-}
module Main where

import UploadGamesCommon

main = runGUI [getDirFromArgs]

