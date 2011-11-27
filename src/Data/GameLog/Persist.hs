{-# LANGUAGE TemplateHaskell #-}
module Data.GameLog.Persist where

import Database.Persist.Base

import Data.GameLog.Types
import Data.GameLog.Aeson
import Data.GameLog.PersistTH

import Control.Applicative
import Language.Haskell.TH

instance (PersistField a) => PersistField (List a) where
    toPersistValue (List vals) = PersistList . map toPersistValue $ vals
    fromPersistValue (PersistList vals) = List <$>  mapM fromPersistValue vals
    sqlType a = sqlType $ toPersistValue a
    isNullable a = isNullable $ toPersistValue a

-- $(derivePersistFieldFromJSON ''List)
$(derivePersistFieldFromJSON ''Spell)
$(derivePersistFieldFromJSON ''PointsPenalty)
$(derivePersistFieldFromJSON ''StatCategory)
$(derivePersistFieldFromJSON ''Statistic)
$(derivePersistFieldFromJSON ''PlayerStats)
$(derivePersistFieldFromJSON ''GameStats)
