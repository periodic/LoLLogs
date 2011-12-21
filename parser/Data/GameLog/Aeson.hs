{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Data.GameLog.Aeson where

import Data.GameLog.Types

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Data.Text (Text)

instance (FromJSON a) => FromJSON (List a) where
    parseJSON (Object obj) = case HM.lookup "list" obj of
                                Just (Object obj) -> List <$> obj .: "source"
                                v                 -> fail $ "Got an invalid type to List: " ++ show v
    parseJSON (Array arr)  = List <$> mapM parseJSON (V.toList arr)
    parseJSON v            = fail $ "Got an invalid type to List: " ++ show v

instance (ToJSON a) => ToJSON (List a) where
    toJSON (List vals) = toJSON vals

$(deriveJSON (drop 5) ''Spell)
$(deriveJSON (drop 1) ''PointsPenalty)
$(deriveJSON (drop 2) ''GameStats)
$(deriveJSON (drop 2) ''PlayerStats)

rawGames = Data.Aeson.Types.parse parseRawGames

parseRawGames (Array arr) = mapM parseRawGame . V.toList $ arr
parseRawGames v           = fail $ "Got an invalid type to GameStats: " ++ show v

parseRawGame (Object obj) = GameStats  
    <$> obj .: "basePoints"
    <*> obj .: "boostIpEarned" <*> obj .: "boostXpEarned"
    <*> obj .: "completionBonusPoints"
    <*> obj .: "difficulty"
    <*> obj .: "elo"
    <*> obj .: "eloChange"
    <*> obj .: "experienceEarned"
    <*> obj .: "experienceTotal"
    <*> obj .: "expPointsToNextLevel"
    <*> obj .: "firstWinBonus"
    <*> obj .: "gameId"
    <*> obj .: "gameLength"
    <*> obj .: "gameMode"
    <*> obj .: "gameType"
    <*> obj .: "imbalancedTeamsNoPoints"
    <*> obj .: "invalid"
    <*> obj .: "ipEarned"
    <*> obj .: "ipTotal"
    <*> obj .: "leveledUp"
    <*> obj .: "locationBoostIpEarned"
    <*> obj .: "locationBoostXpEarned"
    <*> obj .: "loyaltyBoostIpEarned"
    <*> obj .: "loyaltyBoostXpEarned"
    <*>(unboxList <$> obj .: "newSpells")
    <*> obj .: "odinBonusIp"
    <*>(obj .: "otherTeamPlayerParticipantStats" >>= makePlayerMap)
    <*>(unboxList <$>  obj .: "pointsPenalties")
    <*> obj .: "practiceMinutesLeftToday"
    <*> obj .: "practiceMinutesPlayedToday"
    <*> obj .: "practiceMsecsUntilReset"
    <*> obj .: "queueBonusEarned"
    <*> obj .: "queueType"
    <*> obj .: "ranked"
    <*> obj .: "skinIndex"
    <*> obj .: "skinName"
    <*> obj .: "talentPointsGained"
    <*>(obj .: "teamPlayerParticipantStats" >>= makePlayerMap)
    <*> obj .: "timeUntilNextFirstWinBonus"
    <*> obj .: "userId"
    where
        makePlayerMap obj = parseJSON obj >>= listToMap
        listToMap (List arr) = M.fromList . map (\p -> (ps_summonerName p, p)) <$> mapM parseRawPlayer arr
parseRawGame v            = fail $ "Got an invalid type to GameStats: " ++ show v

parseRawPlayer (Object obj) = PlayerStats
    <$> obj .: "_profileIconId"
    <*> obj .: "_summonerName"
    <*> obj .: "botPlayer"
    <*> obj .: "elo"
    <*> obj .: "eloChange"
    <*> obj .: "gameId"
    <*> (unboxList <$> obj .: "gameItems")
    <*> obj .: "inChat"
    <*> obj .: "isMe"
    <*> obj .: "level"
    <*> obj .: "leaver"
    <*> obj .: "leaves"
    <*> obj .: "losses"
    <*> obj .: "profileIconId"
    <*> obj .: "spell1Id"
    <*> obj .: "spell2Id"
    <*> obj .: "skinName"
    <*>(obj .: "statistics" >>= makeStatisticsMap)
    <*> obj .: "teamId"
    <*> obj .: "userId"
    <*> obj .: "wins"
    where
        makeStatisticsMap obj@(Object _) = parseJSON obj >>= listToMap
        makeStatisticsMap v              = fail $ "Got an invalid type to Statistic List: " ++ show v
        listToMap (List arr) = M.fromList <$> mapM makeStatistic arr
        makeStatistic (Object obj) = (,) <$> obj .: "displayName" 
                                         <*> obj .: "value"
        makeStatistic v            = fail $ "Got an invalid type to Statistic: " ++ show v
parseRawPlayer v            = fail $ "Got an invalid type to PlayerStats: " ++ show v

unboxList :: List a -> [a]
unboxList (List ls) = ls
