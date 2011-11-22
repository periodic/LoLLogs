{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, GADTs, OverloadedStrings #-}
module Data.GameLog.Persist where

import Database.Persist
import Database.Persist.Base
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH.Syntax (Type(..))
import Data.GameLog.Types
import Data.Either
import Control.Applicative

instance (PersistField a) => PersistField (List a) where
    toPersistValue (List vals) = PersistList . map toPersistValue $ vals
    fromPersistValue (PersistList vals) = List <$>  mapM fromPersistValue vals
    sqlType a = sqlType $ toPersistValue a
    isNullable a = isNullable $ toPersistValue a

derivePersistField "Spell"
derivePersistField "PointsPenalty"
derivePersistField "StatCategory"
derivePersistField "Statistic"
derivePersistField "PlayerStats"
derivePersistField "GameStats"


