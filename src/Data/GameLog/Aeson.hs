{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Data.GameLog.Aeson where

import Data.GameLog.Types

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as M
import qualified Data.Vector as V


instance (FromJSON a) => FromJSON (List a) where
    parseJSON (Object obj) = case M.lookup "list" obj of
                                Just (Object obj) -> List <$> obj .: "source"
                                v                 -> fail $ "Got an invalid type to List: " ++ show v
    parseJSON (Array arr)  = List <$> mapM parseJSON (V.toList arr)
    parseJSON v            = fail $ "Got an invalid type to List: " ++ show v

instance (ToJSON a) => ToJSON (List a) where
    toJSON (List vals) = toJSON vals


$(deriveJSON (drop 5) ''Spell)
$(deriveJSON (drop 1) ''PointsPenalty)
$(deriveJSON (drop 7) ''StatCategory)
$(deriveJSON (drop 4) ''Statistic)
$(deriveJSON (drop 2) ''PlayerStats)
$(deriveJSON (drop 2) ''GameStats)

