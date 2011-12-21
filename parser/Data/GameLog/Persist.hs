{-# LANGUAGE TemplateHaskell #-}
module Data.GameLog.Persist where

import Database.Persist.Base

import Data.GameLog.Types
import Data.GameLog.Aeson
import Data.GameLog.PersistTH

import Control.Applicative
import Language.Haskell.TH

$(derivePersistFieldFromJSON ''Spell)
$(derivePersistFieldFromJSON ''PointsPenalty)
$(derivePersistFieldFromJSON ''PlayerStats)
$(derivePersistFieldFromJSON ''GameStats)
