{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Model.Game.Query ( Model.Game.Query
                        , Model.Helper.MapReduce
                        ) where

import Prelude
import Database.MongoDB.MapReduceHelper
import Data.UString as US
import Data.GameLog
import Data.Text as T (Text, unpack, pack)
import Data.Bson

import Model.Helper.MapReduce

data Queryable (GameGeneric backend) where
    data QueryColumn (GameGeneric backend) typ
        = typ ~ Text => QGameQueueType
        | typ ~ Int  => QGameLength

data QueryGameQueueType = QueryGameQueueType
instance QueryColumn QueryGameQueueType UString where
    querySelector _ = "this.gameStats.queueType"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "queueType"
    queryFilterSelector _ = "gameStats.queueType"

data QueryGameLength = QueryGameLength
instance QueryColumn QueryGameLength Int where
    querySelector _ = "this.gameStats.gameLength"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "length"
    queryFilterSelector _ = "gameStats.gameLength"

data QueryGameWins = QueryGameWins Text
instance QueryColumn QueryGameWins UString where
    querySelector (QueryGameWins name) = summonerSelector name ".statistics.Victories"
    queryReduceOp _ = GroupTotal
    queryColumnName _ = "wins"

data QueryGameWinPct = QueryGameWinPct Text
instance QueryColumn QueryGameWinPct UString where
    querySelector (QueryGameWinPct name) = summonerSelector name ".statistics.Victories"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "winPct"

data QueryGameSummonerName = QueryGameSummonerName Text
instance QueryColumn QueryGameSummonerName UString where
    querySelector (QueryGameSummonerName name) = summonerSelector name "._summonerName"
    queryReduceOp _ = GroupFirst
    queryColumnName _ = "summonerName"
    queryFilter (QueryGameSummonerName name) val = summonerFilter name "_summonerName" val

data QueryGameSummonerChampion = QueryGameSummonerChampion Text
instance QueryColumn QueryGameSummonerChampion UString where
    querySelector (QueryGameSummonerChampion name) = summonerSelector name ".skinName"
    queryReduceOp _ = GroupFirst
    queryColumnName _ = "champion"
    queryFilter (QueryGameSummonerChampion name) val = summonerFilter name "skinName" val

data QueryGameSummonerKills = QueryGameSummonerKills Text
instance QueryColumn QueryGameSummonerKills UString where
    querySelector (QueryGameSummonerKills name) = summonerSelector name ".statistics['Champion Kills']"
    queryReduceOp _ = GroupTotal
    queryColumnName _ = "kills"

data QueryGameSummonerAvgKills = QueryGameSummonerAvgKills Text
instance QueryColumn QueryGameSummonerAvgKills UString where
    querySelector (QueryGameSummonerAvgKills name) = summonerSelector name ".statistics['Champion Kills']"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "avgKills"

summonerSelector textName subSelector = 
        let name = US.pack (T.unpack textName)
        in US.concat [ "(  this.gameStats.teamPlayerParticipantStats.", name
                     , "&& this.gameStats.teamPlayerParticipantStats.", name, subSelector
                     , ") ||"
                     , "(  this.gameStats.otherTeamPlayerParticipantStats.", name
                     , "&& this.gameStats.otherTeamPlayerParticipantStats.", name, subSelector
                     , ")"
                     ]

summonerFilter textName subSelector v = 
        let name = US.pack (T.unpack textName)
        in [ "$or" =: [ [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      , [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      ]]
