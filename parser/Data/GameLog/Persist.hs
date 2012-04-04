{-# LANGUAGE TemplateHaskell #-}
module Data.GameLog.Persist where

import Control.Applicative
import Language.Haskell.TH

import Database.Persist

import Data.GameLog.Types
import Data.GameLog.Aeson
import Data.GameLog.PersistTH

$(derivePersistFieldFromJSON ''Spell)
$(derivePersistFieldFromJSON ''PointsPenalty)
$(derivePersistFieldFromJSON ''PlayerStats)
$(derivePersistFieldFromJSON ''GameStats)
