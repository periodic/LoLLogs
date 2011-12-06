{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Data.GameLog.MongoDB where

import Database.MongoDB.GameLogs
import Data.UString as US
import Data.GameLog.Types
import Data.Text as T (Text, unpack, pack)
import Data.Bson

data GameQueueType = GameQueueType
instance QueryColumn GameQueueType UString where
    querySelector _ = "this.gameStats.queueType"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "queueType"
    queryFilterSelector _ = "gameStats.queueType"

data GameLength = GameLength
instance QueryColumn GameLength Int where
    querySelector _ = "this.gameStats.gameLength"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "length"
    queryFilterSelector _ = "gameStats.gameLength"

data GameWins = GameWins Text
instance QueryColumn GameWins UString where
    querySelector (GameWins name) = summonerSelector name "statistics.Victories"
    queryReduceOp _ = GroupTotal
    queryColumnName _ = "wins"

data GameWinPct = GameWinPct Text
instance QueryColumn GameWinPct UString where
    querySelector (GameWinPct name) = summonerSelector name "statistics.Victories"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "winPct"

data GameSummonerName = GameSummonerName Text
instance QueryColumn GameSummonerName UString where
    querySelector (GameSummonerName name) = summonerSelector name "_summonerName"
    queryReduceOp _ = GroupFirst
    queryColumnName _ = "summonerName"
    queryFilter (GameSummonerName name) val = summonerFilter name "_summonerName" val

summonerSelector textName subSelector = 
        let name = US.pack (T.unpack textName)
        in US.concat [ "(  this.gameStats.teamPlayerParticipantStats.", name
                     , "&& this.gameStats.teamPlayerParticipantStats.", name, ".", subSelector
                     , ") ||"
                     , "(  this.gameStats.otherTeamPlayerParticipantStats.", name
                     , "&& this.gameStats.otherTeamPlayerParticipantStats.", name, ".", subSelector
                     , ")"
                     ]

summonerFilter textName subSelector v = 
        let name = US.pack (T.unpack textName)
        in [ "$or" =: [ [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      , [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      ]]
