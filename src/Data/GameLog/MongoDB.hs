{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses #-}
module Data.GameLog.MongoDB where

import Database.MongoDB.GameLogs
import Data.UString as US
import Data.GameLog.Types
import Data.Text as T (Text, unpack, pack)

data GameQueueType = GameQueueType
instance QueryColumn GameQueueType Text where
    querySelector _ = "this.gameStats.queueType"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "queueType"

data GameLength = GameLength
instance QueryColumn GameLength Int where
    querySelector _ = "this.gameStats.gameLength"
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "length"

data GameWins = GameWins Text
instance QueryColumn GameWins Text where
    querySelector (GameWins summoner) = US.concat [ "(  this.gameStats.teamPlayerParticipantStats.", US.pack (T.unpack summoner)
                                                  , "&& this.gameStats.teamPlayerParticipantStats.", US.pack (T.unpack summoner), ".statistics.Victories"
                                                  , ") ||"
                                                  , "(  this.gameStats.otherTeamPlayerParticipantStats.", US.pack (T.unpack summoner)
                                                  , "&& this.gameStats.otherTeamPlayerParticipantStats.", US.pack (T.unpack summoner), ".statistics.Victories"
                                                  , ") || 0"]
    queryReduceOp _ = GroupTotal
    queryColumnName _ = "wins"

data GameWinPct = GameWinPct Text
instance QueryColumn GameWinPct Text where
    querySelector (GameWinPct summoner) = US.concat [ "(  this.gameStats.teamPlayerParticipantStats.", US.pack (T.unpack summoner)
                                                    , "&& this.gameStats.teamPlayerParticipantStats.", US.pack (T.unpack summoner), ".statistics.Victories"
                                                    , ") ||"
                                                    , "(  this.gameStats.otherTeamPlayerParticipantStats.", US.pack (T.unpack summoner)
                                                    , "&& this.gameStats.otherTeamPlayerParticipantStats.", US.pack (T.unpack summoner), ".statistics.Victories"
                                                    , ") || 0"]
    queryReduceOp _ = GroupAvg
    queryColumnName _ = "winPct"
