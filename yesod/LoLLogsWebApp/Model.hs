{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, MultiParamTypeClasses #-}
module Model (module Model, module Model.Game) where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.MongoDB
import Language.Haskell.TH.Syntax

import Model.Game

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }, mkMigrate "migrateAll"] $(persistFile "config/models")