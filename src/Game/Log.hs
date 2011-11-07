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
                                v                 -> fail $ "Got an invalid type to List: " ++ show v
    parseJSON (Array arr)  = List <$> mapM parseJSON (V.toList arr)
    parseJSON v            = fail $ "Got an invalid type to List: " ++ show v

instance (ToJSON a) => ToJSON (List a) where
    toJSON (List vals) = toJSON vals

data PointsPenalty = PointsPenalty  { ptype :: String
                                    , ppenalty :: Float
                                    } deriving (Show, Eq)
{-
pointsPenalties = (mx.collections::ArrayCollection)#313
  filterFunction = (null)
  length = 1
  list = (mx.collections::ArrayList)#314
    length = 1
    source = (Array)#315
      [0] (com.riotgames.platform.gameclient.domain::PointsPenalty)#316
        penalty = -0.25
        type = "GAME_TYPE"
    uid = "E446DDDA-82FA-353A-CDEF-7C0D35887DD9"
  sort = (null)
  source = (Array)#315
  -}

data Spell = Spell { spellName :: String
                   }
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

