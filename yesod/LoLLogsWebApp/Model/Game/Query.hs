{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Model.Game.Query ( module Model.Game.Query
                        , module Model.Helper.MapReduce
                        ) where

import Prelude
import Data.UString as US
import Data.Text (Text)
import Data.Bson

import Model.Game
import Model.Helper.MapReduce

instance Queryable (GameGeneric backend) where
    data QueryColumn (GameGeneric backend) typ
        = typ ~ Text    => QGameQueueType      -- game type, as a string
        | typ ~ Int     => QGameLength         -- game length in seconds.
        | typ ~ Text    => QGameChampion Text  -- Champion name, used for filtering and as a key mostly.
        | typ ~ Int     => QGameWins Text    -- Wins total
        | typ ~ Double  => QGameWinPct Text  -- % wins
        | typ ~ Int     => QGameKills Text   -- Total kills
        | typ ~ Int     => QGameDeaths Text  -- Total deaths
        | typ ~ Int     => QGameAssists Text -- Total Assists
        | typ ~ Double  => QGameKPM Text     -- Kills/min
        | typ ~ Double  => QGameDPM Text     -- Deaths/min
        | typ ~ Double  => QGameAPM Text     -- Assists/min
        | typ ~ Int     => QGameCS Text      -- Creep score (neutral + minions)
        | typ ~ Double  => QGameCSPM Text    -- CS/min
        | typ ~ Int     => QGameGold Text    -- Gold
        | typ ~ Double  => QGameGPM Text     -- Gold / minute

    -- querySelector       :: QueryColumn model typ -> Javascript -- ^ Used to select when used as a key.
    queryKeyCode c@QGameQueueType = simpleKey "gameStats.queueType" c
    queryKeyCode c@QGameLength    = simpleKey "gameStats.gameLength" c
    queryKeyCode (QGameChampion  champT) = summonerKeyCode champT ".skinName"
    queryKeyCode (QGameWins    champT) = summonerKeyCode champT ".statistics.Victories" -- Wins total
    queryKeyCode (QGameWinPct  champT) = summonerKeyCode champT ".statistics.Victories" -- % wins
    queryKeyCode (QGameKills   champT) = summonerKeyCode champT ".statistics['Champion Kills']" -- Total kills
    queryKeyCode (QGameDeaths  champT) = summonerKeyCode champT ".statistics.Deaths" -- Total deaths
    queryKeyCode (QGameAssists champT) = summonerKeyCode champT ".statistics.Assists" -- Total Assists
    queryKeyCode (QGameKPM     champT) = summonerKeyCode champT ".statistics['Champion Kills'] * 60 / this.gameStats.gameLength" -- Kills/min
    queryKeyCode (QGameDPM     champT) = summonerKeyCode champT ".statistics.Deaths * 60 / this.gameStats.gameLength" -- Deaths/min
    queryKeyCode (QGameAPM     champT) = summonerKeyCode champT ".statistics.Assists * 60 / this.gameStats.gameLength" -- Assists/min
    queryKeyCode (QGameGold    champT) = summonerKeyCode champT ".statistics['Gold Earned']" -- Gold
    queryKeyCode (QGameGPM     champT) = summonerKeyCode champT ".statistics['Gold Earned'] * 60 / this.gameStats.gameLength" -- Gold / minute
    queryKeyCode (QGameCS      champT) = -- Creep score (neutral + minions)
        let champ = t2u champT
         in wrapJS $ US.concat [ "(this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               , "|| (this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               ]
    queryKeyCode (QGameCSPM    champT) = -- CS/min
        let champ = t2u champT
         in wrapJS $ US.concat [ "(this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               , "|| (this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               ]

    -- queryColumnName     :: QueryColumn model typ -> UString    -- ^ The column name 
    queryColumnName QGameQueueType     = "queueType"
    queryColumnName QGameLength        = "gameLength"
    queryColumnName (QGameChampion  _) = "champion"
    queryColumnName (QGameWins      _) = "wins"
    queryColumnName (QGameWinPct    _) = "winPct"
    queryColumnName (QGameKills     _) = "kills"
    queryColumnName (QGameDeaths    _) = "deaths"
    queryColumnName (QGameAssists   _) = "assists"
    queryColumnName (QGameKPM       _) = "kpm"
    queryColumnName (QGameDPM       _) = "dpm"
    queryColumnName (QGameAPM       _) = "apm"
    queryColumnName (QGameGold      _) = "gold"
    queryColumnName (QGameGPM       _) = "gpm"
    queryColumnName (QGameCS        _) = "cs"
    queryColumnName (QGameCSPM      _) = "cspm"

    -- queryMapCode        :: QueryColumn model typ -> Javascript -- ^ Should set fields on "result" from "this".  Run once for each document.
    queryMapCode c@QGameQueueType = simpleMap "gameStats.queueType" c
    queryMapCode c@QGameLength    = simpleMap "gameStats.gameLength" c
    queryMapCode c@(QGameChampion  champT) = summonerMapCode champT (queryColumnName c) ".skinName"
    queryMapCode c@(QGameWins    champT) = summonerMapCode champT (queryColumnName c) ".statistics.Victories" -- Wins total
    queryMapCode c@(QGameWinPct  champT) = summonerMapCode champT (queryColumnName c) ".statistics.Victories" -- % wins
    queryMapCode c@(QGameKills   champT) = summonerMapCode champT (queryColumnName c) ".statistics['Champion Kills']" -- Total kills
    queryMapCode c@(QGameDeaths  champT) = summonerMapCode champT (queryColumnName c) ".statistics.Deaths" -- Total deaths
    queryMapCode c@(QGameAssists champT) = summonerMapCode champT (queryColumnName c) ".statistics.Assists" -- Total Assists
    queryMapCode c@(QGameKPM     champT) = summonerMapCode champT (queryColumnName c) ".statistics['Champion Kills'] * 60 / this.gameStats.gameLength" -- Kills/min
    queryMapCode c@(QGameDPM     champT) = summonerMapCode champT (queryColumnName c) ".statistics.Deaths * 60 / this.gameStats.gameLength" -- Deaths/min
    queryMapCode c@(QGameAPM     champT) = summonerMapCode champT (queryColumnName c) ".statistics.Assists * 60 / this.gameStats.gameLength" -- Assists/min
    queryMapCode c@(QGameGold    champT) = summonerMapCode champT (queryColumnName c) ".statistics['Gold Earned']" -- Gold
    queryMapCode c@(QGameGPM     champT) = summonerMapCode champT (queryColumnName c) ".statistics['Gold Earned'] * 60 / this.gameStats.gameLength" -- Gold / minute
    queryMapCode c@(QGameCS champT) = -- Creep score (neutral + minions)
        let champ = t2u champT
            field = queryColumnName c
         in wrapJS $ US.concat [ "result.", field, " = "
                               , "(this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               , "|| (this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               ]
    queryMapCode c@(QGameCSPM champT) = -- CS/min
        let champ = t2u champT
            field = queryColumnName c
         in wrapJS $ US.concat [ "result.", field, " = "
                               , "(this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               , "|| (this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               ]

    --queryFilter         :: QueryColumn model typ -> Value -> Document  -- ^ Produce the document to be used as a filter when given a value.
    queryFilter c@QGameQueueType = simpleFilter "gameStats.queueType" c
    queryFilter c@QGameLength    = simpleFilter "gameStats.gameLength" c
    queryFilter (QGameChampion  champT) = summonerFilter champT ".skinName"
    queryFilter (QGameWins    champT) = summonerFilter champT ".statistics.Victories" -- Wins total
    queryFilter (QGameWinPct  champT) = undefined -- TODO
    queryFilter (QGameKills   champT) = undefined -- TODO
    queryFilter (QGameDeaths  champT) = undefined -- TODO
    queryFilter (QGameAssists champT) = undefined -- TODO
    queryFilter (QGameKPM     champT) = undefined -- TODO
    queryFilter (QGameDPM     champT) = undefined -- TODO
    queryFilter (QGameAPM     champT) = undefined -- TODO
    queryFilter (QGameGold    champT) = undefined -- TODO
    queryFilter (QGameGPM     champT) = undefined -- TODO
    queryFilter (QGameCS      champT) = undefined -- TODO
    queryFilter (QGameCSPM    champT) = undefined -- TODO

    -- queryFinalizeCode   :: QueryColumn model typ -> Javascript -- ^ Used to finalize the result.  Should work on "result" and "v".  The value assigned
    queryFinalizeCode c@(QGameChampion t)   = simpleFinalize c
    queryFinalizeCode c                     = simpleFinalizeAvg c


summonerKeyCode :: Text -> UString -> Javascript
summonerKeyCode textName subSelector = 
        let name = t2u textName
         in wrapJS $ US.concat [ "(this.gameStats.teamPlayerParticipantStats['", name, "']", subSelector
                               , "|| this.gameStats.otherTeamPlayerParticipantStats['", name, "']", subSelector
                               , ")"
                               ]

summonerMapCode :: Text -> UString -> UString -> Javascript
summonerMapCode textName field subSelector = 
        let name = t2u textName
        in wrapJS $ US.concat [ "result.", field, " = "
                              , "(  this.gameStats.teamPlayerParticipantStats['", name, "']"
                              , "&& this.gameStats.teamPlayerParticipantStats['", name, "']", subSelector
                              , ") ||"
                              , "(  this.gameStats.otherTeamPlayerParticipantStats['.", name, "']"
                              , "&& this.gameStats.otherTeamPlayerParticipantStats['.", name, "']", subSelector
                              , ")"
                              ]

summonerFilter :: Text -> UString -> Value -> Document
summonerFilter textName subSelector v = 
        let name = t2u textName
        in [ "$or" =: [ [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      , [US.concat ["gameStats.teamPlayerParticipantStats.", name, ".", subSelector] := v]
                      ]]
