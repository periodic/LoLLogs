{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Model.Game.Query ( module Model.Game.Query
                        , module Model.Helper.MapReduce
                        ) where

import Prelude
import Data.UString as US
import Data.GameLog
import Data.Text as T (Text, unpack, pack)
import Data.Bson

import Model.Game
import Model.Helper.MapReduce

instance Queryable (GameGeneric backend) where
    data QueryColumn (GameGeneric backend) typ
        = typ ~ Text => QGameQueueType
        | typ ~ Int  => QGameLength
        | typ ~ Text => QGameChampion Text
    -- querySelector       :: QueryColumn model typ -> Javascript -- ^ Used to select when used as a key.
    queryKeyCode c@QGameQueueType = simpleKey "gameStats.queueType" c
    queryKeyCode c@QGameLength    = simpleKey "gameStats.gameLength" c
    queryKeyCode c@(QGameChampion champT) = 
        let champ = t2u champT
         in wrapJS $ US.concat [ "(this.gameStats.teamPlayerParticipantStats.", champ, ".skinName"
                               , "|| this.gameStats.otherTeamPlayerParticipantStats.", champ, ".skinName"
                               , ")"
                               ]

    -- queryColumnName     :: QueryColumn model typ -> UString    -- ^ The column name 
    queryColumnName QGameQueueType = "queueType"
    queryColumnName QGameLength    = "gameLength"
    queryColumnName (QGameChampion _) = "champion"

    -- queryMapCode        :: QueryColumn model typ -> Javascript -- ^ Should set fields on "result" from "this".  Run once for each document.
    queryMapCode c@QGameQueueType = simpleMap "gameStats.queueType" c
    queryMapCode c@QGameLength    = simpleMap "gameStats.gameLength" c
    queryMapCode c@(QGameChampion champT) = summonerMapCode champT ".skinname"

    --queryFilter         :: QueryColumn model typ -> Value -> Document  -- ^ Produce the document to be used as a filter when given a value.
    queryFilter c@QGameQueueType = simpleFilter "gameStats.queueType" c
    queryFilter c@QGameLength    = simpleFilter "gameStats.gameLength" c
    queryFilter c@(QGameChampion champT) = summonerFilter champT ".skinname"

    -- queryFinalizeCode   :: QueryColumn model typ -> Javascript -- ^ Used to finalize the result.  Should work on "result" and "v".  The value assigned
    queryFinalizeCode c@QGameLength = simpleFinalizeAvg c
    queryFinalizeCode c             = simpleFinalize c

{-
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
-}

summonerMapCode textName subSelector = 
        let name = t2u textName
        in wrapJS $ US.concat [ "(  this.gameStats.teamPlayerParticipantStats.", name
                              , "&& this.gameStats.teamPlayerParticipantStats.", name, subSelector
                              , ") ||"
                              , "(  this.gameStats.otherTeamPlayerParticipantStats.", name
                              , "&& this.gameStats.otherTeamPlayerParticipantStats.", name, subSelector
                              , ")"
                              ]

summonerFilter textName subSelector v = 
        let name = t2u textName
        in [ "$or" =: [ [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      , [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      ]]
