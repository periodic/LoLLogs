{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, TypeFamilies, GADTs #-}
module Main where

import UploadGamesCommon
import UploadGamesGTK

main = runUI [getDirFromArgs, getDirFromDialog]

