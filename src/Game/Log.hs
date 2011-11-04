{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Game.Log where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map as M
import qualified Data.Vector as V

import Control.Applicative
import Control.Monad

data List a = List  { lsource :: [a]
                    } deriving (Show, Eq)
instance (FromJSON a) => FromJSON (List a) where
    parseJSON (Object obj) = case M.lookup "list" obj of
                                Just (Object obj) -> List <$> obj .: "source"
                                _                 -> mzero
    parseJSON (Array arr)  = return . List . V.toList <$> arr
    parseJSON _            = mzero

instance (ToJSON a) => ToJSON (List a) where
    toJSON (List vals) = toJSON vals
{- {
    "filterFunction": null,
    "length": 0,
    "list": {
        "length": 0,
        "source": [],
        "uid": "81BE63BA-4F76-D337-436A-3983678AA90C"
    },
    "sort": null,
    "source": []
}, -}


data PointsPenalty = PointsPenalty  { ptype :: String
                                    , ppenalty :: Float
                                    } deriving (Show, Eq)


data Spell = Spell
             deriving (Show, Eq)

data GameStats = GameStats  { gsbasePoints                        :: Integer
                            , gsboostIpEarned                     :: Integer
                            , gsboostXpEarned                     :: Integer
                            , gscompletionBonusPoints             :: Integer
                            , gsdifficulty                        :: Maybe String
                            , gselo                               :: Integer
                            , gseloChange                         :: Integer
                            , gsexperienceEarned                  :: Integer
                            , gsexperienceTotal                   :: Integer
                            , gsexpPointsToNextLevel              :: Integer
                            , gsfirstWinBonus                     :: Integer
                            , gsgameId                            :: Integer
                            , gsgameLength                        :: Integer
                            , gsgameMode                          :: Maybe String
                            , gsgameType                          :: String
                            , gsimbalancedTeamsNoPoints           :: Bool
                            , gsinvalid                           :: Bool
                            , gsipEarned                          :: Integer
                            , gsipTotal                           :: Integer
                            , gsleveledUp                         :: Bool
                            , gslocationBoostIpEarned             :: Integer
                            , gslocationBoostXpEarned             :: Integer
                            , gsloyaltyBoostIpEarned              :: Integer
                            , gsloyaltyBoostXpEarned              :: Integer
                            , gsnewSpells                         :: List Spell
                            , gsodinBonusIp                       :: Integer
                            , gsotherTeamPlayerParticipantStats   :: List PlayerStats
                            , gspointsPenalties                   :: List PointsPenalty
                            , gspracticeMinutesLeftToday          :: Integer
                            , gspracticeMinutesPlayedToday        :: Maybe Integer
                            , gspracticeMsecsUntilReset           :: Integer
                            , gsqueueBonusEarned                  :: Integer
                            , gsqueueType                         :: String
                            , gsranked                            :: Bool
                            , gsskinIndex                         :: Integer
                            , gsskinName                          :: Maybe String
                            , gstalentPointsGained                :: Integer
                            , gsteamPlayerParticipantStats        :: List PlayerStats
                            , gstimeUntilNextFirstWinBonus        :: Integer
                            , gsuserId                            :: Integer
                            } deriving (Show, Eq)

data PlayerStats = PlayerStats  { ps_profileIconId    :: Integer
                                , ps_summonerName     :: String
                                , psbotPlayer         :: Bool
                                , pselo               :: Integer
                                , pseloChange         :: Integer
                                , psgameId            :: Integer
                                , psgameItems         :: List Integer
                                , psinChat            :: Bool
                                , psisMe              :: Bool
                                , pslevel             :: Integer
                                , psleaver            :: Bool
                                , psleaves            :: Integer
                                , pslosses            :: Integer
                                , psprofileIconId     :: Integer
                                , psspell1Id          :: Integer
                                , psspell2Id          :: Integer
                                , psskinName          :: Maybe String
                                , psstatistics        :: List Statistic
                                , pssummonerName      :: String
                                , psteamId            :: Integer
                                , psuserId            :: Integer
                                , pswins              :: Integer
                                } deriving (Show, Eq)

data Statistic = Statistic  { statdisplayName   :: String
                            , statpriority      :: Integer
                            , statstatCategory  :: StatCategory
                            , statstatTypeName  :: String
                            , statvalue         :: Integer
                            } deriving (Show, Eq)

data StatCategory = StatCategory { statCatdisplayName :: String
                                 , statCatname        :: String
                                 , statCatpriority    :: Integer
                                 } deriving (Show, Eq)

$(deriveJSON (drop 5) ''Spell)
$(deriveJSON (drop 1) ''PointsPenalty)
$(deriveJSON (drop 7) ''StatCategory)
$(deriveJSON (drop 4) ''Statistic)
$(deriveJSON (drop 2) ''PlayerStats)
$(deriveJSON (drop 2) ''GameStats)

