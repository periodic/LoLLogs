module Data.GameLog.Types where

import Data.Text
import Data.Map

data List a = List  { lSource :: [a]
                    } deriving (Show, Read, Eq)

data PointsPenalty = PointsPenalty  { pType :: Text
                                    , pPenalty :: Float
                                    } deriving (Show, Read, Eq)

data Spell = Spell { spellName :: Text
                   }
             deriving (Show, Read, Eq)

data GameStats = GameStats  { gsBasePoints                        :: Int
                            , gsBlueTeam                          :: [Text]
                            , gsBoostIpEarned                     :: Int
                            , gsBoostXpEarned                     :: Int
                            , gsChampions                         :: [Text]
                            , gsCompletionBonusPoints             :: Int
                            , gsDifficulty                        :: Maybe Text
                            , gsElo                               :: Int
                            , gsEloChange                         :: Int
                            , gsExperienceEarned                  :: Int
                            , gsExperienceTotal                   :: Int
                            , gsExpPointsToNextLevel              :: Int
                            , gsFirstWinBonus                     :: Int
                            , gsGameId                            :: Int
                            , gsGameLength                        :: Int
                            , gsGameMode                          :: Maybe Text
                            , gsGameType                          :: Text
                            , gsImbalancedTeamsNoPoints           :: Bool
                            , gsInvalid                           :: Bool
                            , gsIpEarned                          :: Int
                            , gsIpTotal                           :: Int
                            , gsLeveledUp                         :: Bool
                            , gsLocationBoostIpEarned             :: Int
                            , gsLocationBoostXpEarned             :: Int
                            , gsLoyaltyBoostIpEarned              :: Int
                            , gsLoyaltyBoostXpEarned              :: Int
                            , gsNewSpells                         :: [Spell]
                            , gsOdinBonusIp                       :: Int
                            , gsPlayerStats                       :: Map Text PlayerStats
                            , gsPointsPenalties                   :: [PointsPenalty]
                            , gsPracticeMinutesLeftToday          :: Int
                            , gsPracticeMinutesPlayedToday        :: Maybe Int
                            , gsPracticeMsecsUntilReset           :: Int
                            , gsPurpleTeam                        :: [Text]
                            , gsQueueBonusEarned                  :: Int
                            , gsQueueType                         :: Text
                            , gsRanked                            :: Bool
                            , gsSkinIndex                         :: Int
                            , gsSkinName                          :: Maybe Text
                            , gsSummoners                         :: [Text]
                            , gsTalentPointsGained                :: Int
                            , gsTimeUntilNextFirstWinBonus        :: Int
                            , gsUserId                            :: Int
                            } deriving (Show, Read, Eq)

data PlayerStats = PlayerStats  { ps_profileIconId    :: Int
                                , ps_summonerName     :: Text
                                , psBotPlayer         :: Bool
                                , psElo               :: Int
                                , psEloChange         :: Int
                                , psGameId            :: Int
                                , psGameItems         :: [Int]
                                , psInChat            :: Bool
                                , psIsMe              :: Bool
                                , psLevel             :: Int
                                , psLeaver            :: Bool
                                , psLeaves            :: Int
                                , psLosses            :: Int
                                , psProfileIconId     :: Int
                                , psSpell1Id          :: Int
                                , psSpell2Id          :: Int
                                , psSkinName          :: Text
                                , psStatistics        :: Map Text Int
                                , psTeamId            :: Int
                                , psUserId            :: Int
                                , psWins              :: Int
                                } deriving (Show, Read, Eq)
