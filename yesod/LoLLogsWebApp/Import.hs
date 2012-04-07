module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , (<>)
    , Text
    , module Settings.StaticFiles
    , module Data.Monoid
    , module Control.Applicative
    -- Helper functions
    , roundLargeNumber
    , formatGameTime
    , formatLargeNumber
    ) where

import Prelude hiding (writeFile, readFile)
import Yesod
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Text.Printf
import Settings.StaticFiles

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

roundLargeNumber :: (Integral i) => i -> String
roundLargeNumber i = if i < 1000
                     then show i
                     else if i < 1000000
                        then printf "%.1fk" (fromIntegral i / 1000 :: Float)
                        else printf "%.1fM" (fromIntegral i / 1000000 :: Float)

formatGameTime :: Int -> String
formatGameTime i = let hours = i `div` 3600
                       mins  = (i `div` 60) `mod` 60
                       secs  = i `mod` 60
                    in if hours > 0
                       then printf "%02d:%02d:%02d" hours mins secs
                       else printf "%02d:%02d" mins secs

formatLargeNumber :: (Integral i, PrintfArg i) => i -> String
formatLargeNumber i = let lowPart = i `mod` 1000
                          highPart = i `div` 1000
                       in if highPart == 0
                          then show lowPart
                          else formatLargeNumber highPart ++ "," ++ printf "%03d" lowPart
