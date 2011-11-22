{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, GADTs, OverloadedStrings #-}
module Game.Log.Persist where

import Database.Persist
import Database.Persist.Base
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH.Syntax (Type(..))
import Game.Log

derivePersistField "List"
derivePersistField  "Spell"
derivePersistField  "PointsPenalty"
derivePersistField  "StatCategory"
derivePersistField  "Statistic"
derivePersistField  "PlayerStats"
derivePersistField  "GameStats"

