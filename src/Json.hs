{-# LANGUAGE TemplateHaskell #-}
module Json where

import Data.Aeson
import Data.Aeson.TH

import qualified Data.Vector as V
import qualified Data.Map as M

import Value


instance ToJSON Value.Value where
    toJSON (NumberValue i)          = toJSON i
    toJSON (NaNValue)               = Number (0/0)
    toJSON (StringValue str)        = toJSON  str
    toJSON (BooleanValue b)         = toJSON b
    toJSON (NullValue)              = Null
    toJSON (DateValue d)            = toJSON d
    toJSON (ArrayValue _ elems)     = toJSON $ V.fromList elems
    toJSON (ObjectValue _ _ elems)  = toJSON $ M.fromList elems
    toJSON (MapValue _ elems)       = toJSON $ M.fromList elems
