{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Data.ASObject.Json where

-- Aeson
import Data.Aeson

-- DataTypes
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.Text as T

-- Control flow
import Data.Traversable (traverse)
import Control.Applicative

-- ASValue
import Data.ASObject.Types

instance FromJSON ASValue where
    parseJSON (Object obj)    = ASObject T.empty (-1) . M.fromList . HM.toList <$> traverse parseJSON  obj
    parseJSON (Array arr)     = ASArray          (-1) <$> traverse parseJSON  arr
    parseJSON (String text)   = return $ ASString text
    parseJSON (Number n)      = return $ ASNumber n
    parseJSON (Bool bool)     = return $ ASBoolean bool
    parseJSON (Null)          = return ASNull

instance ToJSON ASValue where
    toJSON (ASObject _ _ obj) = Object . HM.fromList . M.toList $ M.map toJSON obj
    toJSON (ASArray _ vals)   = Array $ V.map toJSON vals
    toJSON (ASString str)     = String str
    toJSON (ASNumber num)     = Number num
    toJSON (ASBoolean bool)   = Bool bool
    toJSON (ASNull)           = Null

