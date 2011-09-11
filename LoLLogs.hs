{-# LANGUAGE OverloadedStrings #-}
module LoLLogs where
import Prelude
import Data.Aeson
import Data.Attoparsec (parse, Result(..))

import Data.ByteString.Char8 as BS

import Control.Applicative
import Control.Monad (mzero)

data GameData = GameData { gameModel :: GameModel
                         , versionNumber :: String
                         , endOfGameStats :: GameStats
                         } deriving (Show, Eq)
instance FromJSON GameData where
    parseJSON (Object v) = GameData <$>
                            v .: "gameModel" <*>
                            v .: "versionNumber" <*>
                            v .: "endOfGameStats"
    parseJSON _ = mzero

data GameMap = GameMap    { mapDescription :: String
                          , mapDisplayName :: String
                          , mapName :: String
                          , mapId :: Integer
                          , mapTotalPlayers :: Integer
                          } deriving (Show, Eq)
instance FromJSON GameMap where
    parseJSON (Object v) = GameMap <$>
                            v .: "description" <*>
                            v .: "displayName" <*>
                            v .: "name" <*>
                            v .: "mapId" <*>
                            v .: "totalPlayers"
    parseJSON _ = mzero

data GameModel = GameModel  { gmGameMap :: GameMap
                            , gmNumPlayersPerTeam :: Integer
                            , gmGameMode :: Maybe String
                            , gmRanked :: Maybe Bool
                            , gmDifficulty :: Maybe String
                            , gmGameType :: String
                            , gmTutorialType :: String
                            } deriving (Show, Eq)
instance FromJSON GameModel where
    parseJSON (Object v) = GameModel <$>
                            v .: "gameMap" <*>
                            v .: "numPlayersPerTeam" <*>
                            v .: "gameMode" <*>
                            v .: "ranked" <*>
                            v .: "difficulty" <*>
                            v .: "gameType" <*>
                            v .: "tutorialType"
    parseJSON _ = mzero

data List a = List  { lUid :: String
                    , lLength :: Integer
                    , lSource :: [a]
                    } deriving (Show, Eq)
instance FromJSON a => FromJSON (List a) where
    parseJSON (Object v) = List <$>
                            v .: "uid" <*>
                            v .: "length" <*>
                            v .: "source"
    parseJSON _ = mzero

data WrappedList a = WrappedList    { wlSource :: [a]
                                    -- , wlList :: List a 
                                    -- , wlFilterFunction :: String
                                    -- , wlSort :: String
                                    , wlLength :: Integer
                                    } deriving (Show, Eq)
instance FromJSON a => FromJSON (WrappedList a) where
    parseJSON (Object v) = WrappedList <$>
                            -- v .: "list" <*>
                            v .: "source" <*>
                            -- v .: "filterFunction" <*>
                            -- v .: "sort" <*>
                            v .: "length"
    parseJSON _ = mzero

data PointsPenalty = PointsPenalty  { pType :: String
                                    , pPenalty :: Float
                                    } deriving (Show, Eq)
instance FromJSON PointsPenalty where
    parseJSON (Object v) = PointsPenalty <$>
                            v .: "type" <*>
                            v .: "penalty"
    parseJSON _ = mzero

data Spell = Spell
             deriving (Show, Eq)
instance FromJSON Spell where
    parseJSON (Object v) = return Spell
    parseJSON _ = mzero


data GameStats = GameStats  { gsGameId                            :: Integer
                            , gsGameLength                        :: Integer
                            , gsGameMode                          :: Maybe String
                            , gsGameType                          :: String
                            , gsRanked                            :: Bool
                            , gsDifficulty                        :: Maybe String

                            , gsIpEarned                          :: Integer
                            , gsIpTotal                           :: Integer
                            , gsBoostIpEarned                     :: Integer
                            , gsFirstWinBonus                     :: Integer

                            , gsSkinName                          :: Maybe String
                            , gsSkinIndex                         :: Integer

                            , gsExperienceTotal                   :: Integer
                            , gsExperienceEarned                  :: Integer
                            , gsExpPointsToNextLevel              :: Integer
                            , gsBoostXpEarned                     :: Integer
                            , gsLeveledUp                         :: Bool

                            , gsNewSpells                         :: WrappedList Spell
                            , gsUserId                            :: Integer
                            , gsQueueType                         :: String
                            , gsInvalid                           :: Bool
                            , gsPointsPenalties                   :: WrappedList PointsPenalty
                            , gsPracticeMsecsUntilReset           :: Integer
                            --, gsPracticeMinutesPlayedToday        :: Maybe Integer
                            , gsTimeUntilNextFirstWinBonus        :: Integer
                            , gsCompletionBonusPoints             :: Integer

                            , gsEloChange                         :: Integer

                            , gsPracticeMinutesLeftToday          :: Integer
                            , gsBasePoints                        :: Integer
                            , gsTeamPlayerParticipantStats        :: WrappedList PlayerStats
                            , gsOtherTeamPlayerParticipantStats   :: WrappedList PlayerStats
                            , gsElo                               :: Integer
                            , gsTalentPointsGained                :: Integer
                            , gsImbalancedTeamsNoPoints           :: Bool
                            , gsOdinBonusIp                       :: Integer
                            } deriving (Show, Eq)
instance FromJSON GameStats where
    parseJSON (Object v) = GameStats <$>
                            v .: "gameId" <*>
                            v .: "gameLength" <*>
                            v .: "gameMode" <*>
                            v .: "gameType" <*>
                            v .: "ranked" <*>
                            v .: "difficulty" <*>

                            v .: "ipEarned" <*>
                            v .: "ipTotal" <*>
                            v .: "boostIpEarned" <*>
                            v .: "firstWinBonus" <*>

                            v .: "skinName" <*>
                            v .: "skinIndex" <*>

                            v .: "experienceTotal" <*>
                            v .: "experienceEarned" <*>
                            v .: "expPointsToNextLevel" <*>
                            v .: "boostXpEarned" <*>
                            v .: "leveledUp" <*>

                            v .: "newSpells" <*>
                            v .: "userId" <*>
                            v .: "queueType" <*>
                            v .: "invalid" <*>
                            v .: "pointsPenalties" <*>
                            v .: "practiceMsecsUntilReset" <*>
                            -- v .: "practiceMinutesPlayedToday" <*>
                            v .: "timeUntilNextFirstWinBonus" <*>
                            v .: "completionBonusPoints" <*>

                            v .: "eloChange" <*>

                            v .: "practiceMinutesLeftToday" <*>
                            v .: "basePoints" <*>
                            v .: "otherTeamPlayerParticipantStats" <*>
                            v .: "teamPlayerParticipantStats" <*>
                            v .: "elo" <*>
                            v .: "talentPointsGained" <*>
                            v .: "imbalancedTeamsNoPoints" <*>
                            v .: "odinBonusIp"
    parseJSON _ = mzero


data PlayerStats = PlayerStats  { ps_profileIconId    :: Integer
                                , ps_summonerName     :: String
                                , psBotPlayer         :: Bool
                                , psElo               :: Integer
                                , psEloChange         :: Integer
                                , psGameId            :: Integer
                                , psGameItems         :: WrappedList Integer
                                , psInChat            :: Bool
                                , psIsMe              :: Bool
                                , psLevel             :: Integer
                                , psLeaver            :: Bool
                                , psLeaves            :: Integer
                                , psLosses            :: Integer
                                , psProfileIconId     :: Integer
                                , psSpell1Id          :: Integer
                                , psSpell2Id          :: Integer
                                , psSkinName          :: Maybe String
                                , psStatistics        :: WrappedList Statistic
                                , psSummonerName      :: String
                                , psTeamId            :: Integer
                                , psUserId            :: Integer
                                , psWins              :: Integer
                                } deriving (Show, Eq)
instance FromJSON PlayerStats where
    parseJSON (Object v) = PlayerStats <$>
                                v .: "_profileIconId" <*>
                                v .: "_summonerName" <*>
                                v .: "botPlayer" <*>
                                v .: "elo" <*>
                                v .: "eloChange" <*>
                                v .: "gameId" <*>
                                v .: "gameItems" <*>
                                v .: "inChat" <*>
                                v .: "isMe" <*>
                                v .: "level" <*>
                                v .: "leaver" <*>
                                v .: "leaves" <*>
                                v .: "losses" <*>
                                v .: "profileIconId" <*>
                                v .: "spell1Id" <*>
                                v .: "spell2Id" <*>
                                v .: "skinName" <*>
                                v .: "statistics" <*>
                                v .: "summonerName" <*>
                                v .: "teamId" <*>
                                v .: "userId" <*>
                                v .: "wins"
    parseJSON _ = mzero

data Statistic = Statistic  { statTypeName      :: String
                            , statDisplayName   :: String
                            , statValue         :: Integer
                            } deriving (Show, Eq)

instance FromJSON Statistic where
    parseJSON (Object v) = Statistic <$>
                            v .: "statTypeName" <*>
                            v .: "displayName" <*>
                            v .: "value"
    parseJSON _ = mzero


parseFile :: FilePath -> IO (Either GameData String)
parseFile path = do
    raw <- BS.readFile path 
    if (BS.take 2 raw == pack "\x06\x8c")
        then return $ doParse (BS.drop 4 raw)
        else return $ doParse raw
    where
        doParse raw = case parse (fromJSON <$> json) raw of 
                        Done _ (Error msg) -> Right msg
                        Done _ (Success p) -> Left p
                        Fail _ _ msg       -> Right msg
                        x                  -> Right . show $ x

getPlayer :: GameData -> Maybe PlayerStats
getPlayer stats = case Prelude.filter (psIsMe) (thisTeam ++ thatTeam) of
                            []      -> Nothing
                            (p:_)   -> Just p
    where
        thisTeam = wlSource . gsTeamPlayerParticipantStats . endOfGameStats $ stats
        thatTeam = wlSource . gsOtherTeamPlayerParticipantStats . endOfGameStats $ stats


getStat :: PlayerStats -> String -> Maybe Integer
getStat p statType = case Prelude.filter ((== statType) . statTypeName) (wlSource . psStatistics $ p) of
                            []      -> Nothing
                            (s:_)   -> Just . statValue $ s

