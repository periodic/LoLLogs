module Data.GameLog.Types where

import Data.Text
import Data.Map

data List a = List  { lsource :: [a]
                    } deriving (Show, Read, Eq)

data PointsPenalty = PointsPenalty  { ptype :: Text
                                    , ppenalty :: Float
                                    } deriving (Show, Read, Eq)

data Spell = Spell { spellName :: Text
                   }
             deriving (Show, Read, Eq)

data GameStats = GameStats  { gsbasePoints                        :: Int
                            , gsboostIpEarned                     :: Int
                            , gsboostXpEarned                     :: Int
                            , gscompletionBonusPoints             :: Int
                            , gsdifficulty                        :: Maybe Text
                            , gselo                               :: Int
                            , gseloChange                         :: Int
                            , gsexperienceEarned                  :: Int
                            , gsexperienceTotal                   :: Int
                            , gsexpPointsToNextLevel              :: Int
                            , gsfirstWinBonus                     :: Int
                            , gsgameId                            :: Int
                            , gsgameLength                        :: Int
                            , gsgameMode                          :: Maybe Text
                            , gsgameType                          :: Text
                            , gsimbalancedTeamsNoPoints           :: Bool
                            , gsinvalid                           :: Bool
                            , gsipEarned                          :: Int
                            , gsipTotal                           :: Int
                            , gsleveledUp                         :: Bool
                            , gslocationBoostIpEarned             :: Int
                            , gslocationBoostXpEarned             :: Int
                            , gsloyaltyBoostIpEarned              :: Int
                            , gsloyaltyBoostXpEarned              :: Int
                            , gsnewSpells                         :: [Spell]
                            , gsodinBonusIp                       :: Int
                            , gsotherTeamPlayerParticipantStats   :: Map Text PlayerStats
                            , gspointsPenalties                   :: [PointsPenalty]
                            , gspracticeMinutesLeftToday          :: Int
                            , gspracticeMinutesPlayedToday        :: Maybe Int
                            , gspracticeMsecsUntilReset           :: Int
                            , gsqueueBonusEarned                  :: Int
                            , gsqueueType                         :: Text
                            , gsranked                            :: Bool
                            , gsskinIndex                         :: Int
                            , gsskinName                          :: Maybe Text
                            , gstalentPointsGained                :: Int
                            , gsteamPlayerParticipantStats        :: Map Text PlayerStats
                            , gstimeUntilNextFirstWinBonus        :: Int
                            , gsuserId                            :: Int
                            } deriving (Show, Read, Eq)

data PlayerStats = PlayerStats  { ps_profileIconId    :: Int
                                , ps_summonerName     :: Text
                                , psbotPlayer         :: Bool
                                , pselo               :: Int
                                , pseloChange         :: Int
                                , psgameId            :: Int
                                , psgameItems         :: [Int]
                                , psinChat            :: Bool
                                , psisMe              :: Bool
                                , pslevel             :: Int
                                , psleaver            :: Bool
                                , psleaves            :: Int
                                , pslosses            :: Int
                                , psprofileIconId     :: Int
                                , psspell1Id          :: Int
                                , psspell2Id          :: Int
                                , psskinName          :: Maybe Text
                                , psstatistics        :: Map Text Int
                                , psteamId            :: Int
                                , psuserId            :: Int
                                , pswins              :: Int
                                } deriving (Show, Read, Eq)

{-
data Statistic = Statistic  { statdisplayName   :: Text
                            , statpriority      :: Int
                            , statstatCategory  :: StatCategory
                            , statstatTypeName  :: Text
                            , statvalue         :: Int
                            } deriving (Show, Read, Eq)

data StatCategory = StatCategory { statCatdisplayName :: Text
                                 , statCatname        :: Text
                                 , statCatpriority    :: Int
                                 } deriving (Show, Read, Eq)
-}
