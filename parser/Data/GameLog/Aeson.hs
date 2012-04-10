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
import Data.Char as C (toLower)
import Data.Text as T (Text, toLower)

instance (FromJSON a) => FromJSON (List a) where
    parseJSON (Object obj) = case HM.lookup "list" obj of
                                Just (Object obj) -> List <$> obj .: "source"
                                v                 -> fail $ "Got an invalid type to List: " ++ show v
    parseJSON (Array arr)  = List <$> mapM parseJSON (V.toList arr)
    parseJSON v            = fail $ "Got an invalid type to List: " ++ show v

instance (ToJSON a) => ToJSON (List a) where
    toJSON (List vals) = toJSON vals

-- This can't be used due to stage restrictions with template haskell.
-- lcFirst (c:cs) = toLower c : cs

$(deriveJSON ((\(c:cs) -> C.toLower c : cs) . drop 5) ''Spell)
$(deriveJSON ((\(c:cs) -> C.toLower c : cs) . drop 1) ''PointsPenalty)
$(deriveJSON ((\(c:cs) -> C.toLower c : cs) . drop 2) ''GameStats)
$(deriveJSON ((\(c:cs) -> C.toLower c : cs) . drop 2) ''PlayerStats)

rawGames = Data.Aeson.Types.parse parseRawGames

parseRawGames (Array arr) = mapM parseRawGame . V.toList $ arr
parseRawGames v           = fail $ "Got an invalid type to GameStats: " ++ show v

parseRawGame (Object obj) = GameStats  
    <$> obj .: "basePoints"
    <*> blueTeam
    <*> obj .: "boostIpEarned" 
    <*> obj .: "boostXpEarned"
    <*> champions
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
    <*> playerStats
    <*>(unboxList <$>  obj .: "pointsPenalties")
    <*> obj .: "practiceMinutesLeftToday"
    <*> obj .: "practiceMinutesPlayedToday"
    <*> obj .: "practiceMsecsUntilReset"
    <*> purpleTeam
    <*> obj .: "queueBonusEarned"
    <*> obj .: "queueType"
    <*> obj .: "ranked"
    <*> obj .: "skinIndex"
    <*> obj .: "skinName"
    <*> summoners
    <*> obj .: "talentPointsGained"
    <*> obj .: "timeUntilNextFirstWinBonus"
    <*> obj .: "userId"
    where
        playerTeam = do
            value <- obj .: "teamPlayerParticipantStats"
            case value of
                List ps -> mapM parseRawPlayer ps
                _       -> fail "teamPlayerParticipantStats is not an array."
        otherTeam = do
            value <- obj .: "otherTeamPlayerParticipantStats"
            case value of
                List ps -> mapM parseRawPlayer ps
                _       -> fail "otherTeamPlayerParticipantStats is not an array."

        summoners = do
            pTeam <- playerTeam
            oTeam <- otherTeam
            return . map ps_summonerName $ pTeam ++ oTeam

        playerStats = do
            pTeam <- playerTeam
            oTeam <- otherTeam
            return . M.fromList . map (\p -> (T.toLower (ps_summonerName p), p)) $ pTeam ++ oTeam

        blueTeam = do
            pTeam <- playerTeam
            oTeam <- otherTeam
            if (==100) . psTeamId . head $ pTeam
                then return . map (T.toLower . ps_summonerName) $ pTeam
                else return . map (T.toLower . ps_summonerName) $ oTeam
        purpleTeam = do
            pTeam <- playerTeam
            oTeam <- otherTeam
            if (==100) . psTeamId . head $ pTeam
                then return . map (T.toLower . ps_summonerName) $ oTeam -- note, opposite of blueTeam above.
                else return . map (T.toLower . ps_summonerName) $ pTeam

        champions = do
            pTeam <- playerTeam
            oTeam <- otherTeam
            return . map psSkinName $ pTeam ++ oTeam
        -- listToMap (List arr) = M.fromList . map (\p -> (ps_summonerName p, p)) <$> mapM parseRawPlayer arr




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
