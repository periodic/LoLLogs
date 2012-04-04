{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.GameLog.PersistTH ( derivePersistFieldFromJSON
                              ) where

import Database.Persist.Store
import Database.Persist.TH
import Language.Haskell.TH.Syntax (Type(..))

-- Types needed for JSON version.
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as P
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as E
import qualified Data.Attoparsec.Number as N
import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V
import Control.Monad (liftM)
import Language.Haskell.TH

{- | This module is designed to allow us to create PersistField instances for
 - data types that have FromJSON and ToJSON instances (as implemented in the
 - Aeson package).  It does this by doing two things:  Defining an instance of
 - PersistField for Aeson.Value, and then defining a template-haskell function
 - for defining a PersistField instance using that.  
 -
 - The PersistField definition is very simple.  To get a PersistValue from the
 - data type we first convert it to JSON through toJSON, then convert that to a
 - PersistValue.  To convert a PersistValue to a data type we first convert it
 - to an Aeson.Value type, then parse it using parseJSON/fromJSON.
 -}

data T1 = T1

fmapLeft :: (a -> b) -> Either a c -> Either b c
fmapLeft f (Left a) = Left (f a)
fmapLeft _ (Right a)= Right a -- Rewrap to fix types.

derivePersistFieldFromJSON t = do
    d <- [d| instance PersistField T1 where
                toPersistValue = toPersistValue . toJSON
                fromPersistValue val = fromPersistValue val >>= fmapLeft pack . P.parseEither parseJSON
                sqlType _ = SqlString
                isNullable _ = False
         |]
    let    [InstanceD [] (AppT showt (ConT _T1)) body] = d
    return [InstanceD [] (AppT showt (ConT t  )) body]


{- Instance for the Aeson.Value type. -}
instance PersistField Value where
    toPersistValue (Object obj) = PersistMap . map (\(k,v) -> (k, toPersistValue v)) . M.toList $ obj
    toPersistValue (Array arr)  = PersistList . map toPersistValue . V.toList $ arr
    toPersistValue (String text)= PersistText text
    toPersistValue (Number num) = case num of
                                      N.I int -> PersistInt64 . fromIntegral $ int
                                      N.D double -> PersistDouble double
    toPersistValue (Bool bool)  = PersistBool bool
    toPersistValue (Null)       = PersistNull

    fromPersistValue (PersistText text)     = Right $ String text
    fromPersistValue (PersistByteString bs) = Right . String . E.decodeUtf8 $ bs
    fromPersistValue (PersistInt64 int)     = Right . Number . N.I . fromIntegral $ int
    fromPersistValue (PersistDouble doub)   = Right . Number . N.D $ doub
    fromPersistValue (PersistBool bool)     = Right $ Bool bool
    fromPersistValue (PersistDay day)       = Right . String . pack . show $ day
    fromPersistValue (PersistTimeOfDay time)= Right . String . pack . show $ time
    fromPersistValue (PersistUTCTime utc)   = Right . String . pack . show $ utc
    fromPersistValue (PersistNull)          = Right $ Null
    fromPersistValue (PersistList vals)     = (Array . V.fromList) `liftM` (mapM (fromPersistValue) vals)
    fromPersistValue (PersistMap listPairs) = let parsePair (k,v) = case fromPersistValue v of
                                                                        Right s -> Right (k,s)
                                                                        Left m  -> Left m
                                              in (Object . M.fromList) `liftM` (mapM parsePair listPairs)
    fromPersistValue (PersistObjectId bs)   = Right . String . E.decodeUtf8 $ bs

    sqlType    _ = SqlString
    isNullable _ = False
