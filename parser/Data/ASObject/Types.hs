{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Data.ASObject.Types ( Object
                           , ASValue(..)
                           ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Attoparsec.Number as N

type Object      = M.Map T.Text ASValue

data ASValue = ASNumber N.Number
             | ASString T.Text
             | ASBoolean !Bool
             | ASNull
             | ASDate T.Text
             | ASArray !Integer (V.Vector ASValue)
             | ASObject T.Text !Integer Object
             deriving (Show, Eq)


