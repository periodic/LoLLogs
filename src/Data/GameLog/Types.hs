{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Data.GameLog.Types where

data List a = List  { lsource :: [a]
                    } deriving (Show, Read, Eq)
data PointsPenalty = PointsPenalty  { ptype :: String
                                    , ppenalty :: Float
                                    } deriving (Show, Read, Eq)

data Spell = Spell { spellName :: String
                   }
             deriving (Show, Read, Eq)

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
                            } deriving (Show, Read, Eq)

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
                                , psteamId            :: Integer
                                , psuserId            :: Integer
                                , pswins              :: Integer
                                } deriving (Show, Read, Eq)

data Statistic = Statistic  { statdisplayName   :: String
                            , statpriority      :: Integer
                            , statstatCategory  :: StatCategory
                            , statstatTypeName  :: String
                            , statvalue         :: Integer
                            } deriving (Show, Read, Eq)

data StatCategory = StatCategory { statCatdisplayName :: String
                                 , statCatname        :: String
                                 , statCatpriority    :: Integer
                                 } deriving (Show, Read, Eq)
