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
        | typ ~ Int     => QGameSummoner Text  -- game length in seconds.
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
    queryKeyCode (QGameSummoner sNameT) = summonerKeyCode sNameT "._summonerName"
    queryKeyCode (QGameChampion sNameT) = summonerKeyCode sNameT ".skinName"
    queryKeyCode (QGameWins    sNameT) = summonerKeyCode sNameT ".statistics.Victories" -- Wins total
    queryKeyCode (QGameWinPct  sNameT) = summonerKeyCode sNameT ".statistics.Victories" -- % wins
    queryKeyCode (QGameKills   sNameT) = summonerKeyCode sNameT ".statistics['Champion Kills']" -- Total kills
    queryKeyCode (QGameDeaths  sNameT) = summonerKeyCode sNameT ".statistics.Deaths" -- Total deaths
    queryKeyCode (QGameAssists sNameT) = summonerKeyCode sNameT ".statistics.Assists" -- Total Assists
    queryKeyCode (QGameKPM     sNameT) = summonerKeyCode sNameT ".statistics['Champion Kills'] * 60 / this.gameStats.gameLength" -- Kills/min
    queryKeyCode (QGameDPM     sNameT) = summonerKeyCode sNameT ".statistics.Deaths * 60 / this.gameStats.gameLength" -- Deaths/min
    queryKeyCode (QGameAPM     sNameT) = summonerKeyCode sNameT ".statistics.Assists * 60 / this.gameStats.gameLength" -- Assists/min
    queryKeyCode (QGameGold    sNameT) = summonerKeyCode sNameT ".statistics['Gold Earned']" -- Gold
    queryKeyCode (QGameGPM     sNameT) = summonerKeyCode sNameT ".statistics['Gold Earned'] * 60 / this.gameStats.gameLength" -- Gold / minute
    queryKeyCode (QGameCS      sNameT) = -- Creep score (neutral + minions)
        let champ = t2u sNameT
         in wrapJS $ US.concat [ "(this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               , "|| (this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               ]
    queryKeyCode (QGameCSPM    sNameT) = -- CS/min
        let champ = t2u sNameT
         in wrapJS $ US.concat [ "(this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               , "|| (this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               ]

    -- queryColumnName     :: QueryColumn model typ -> UString    -- ^ The column name 
    queryColumnName QGameQueueType     = "queueType"
    queryColumnName QGameLength        = "gameLength"
    queryColumnName (QGameSummoner  _) = "summoner"
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
    queryMapCode c@(QGameSummoner  sNameT) = summonerMapCode sNameT (queryColumnName c) "._summonerName"
    queryMapCode c@(QGameChampion  sNameT) = summonerMapCode sNameT (queryColumnName c) ".skinName"
    queryMapCode c@(QGameWins    sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Victories" -- Wins total
    queryMapCode c@(QGameWinPct  sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Victories" -- % wins
    queryMapCode c@(QGameKills   sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Champion Kills']" -- Total kills
    queryMapCode c@(QGameDeaths  sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Deaths" -- Total deaths
    queryMapCode c@(QGameAssists sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Assists" -- Total Assists
    queryMapCode c@(QGameKPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Champion Kills'] * 60 / this.gameStats.gameLength" -- Kills/min
    queryMapCode c@(QGameDPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Deaths * 60 / this.gameStats.gameLength" -- Deaths/min
    queryMapCode c@(QGameAPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Assists * 60 / this.gameStats.gameLength" -- Assists/min
    queryMapCode c@(QGameGold    sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Gold Earned']" -- Gold
    queryMapCode c@(QGameGPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Gold Earned'] * 60 / this.gameStats.gameLength" -- Gold / minute
    queryMapCode c@(QGameCS sNameT) = -- Creep score (neutral + minions)
        let champ = t2u sNameT
            field = queryColumnName c
         in wrapJS $ US.concat [ "result.", field, " = "
                               , "(this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Neutral Monsters Killed'])"
                               , "|| (this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Neutral Monsters Killed'])"
                               ]
    queryMapCode c@(QGameCSPM sNameT) = -- CS/min
        let champ = t2u sNameT
            field = queryColumnName c
         in wrapJS $ US.concat [ "result.", field, " = "
                               , "(this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               , "|| (this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Minions Slain']"
                               , "+ this.gameStats.teamPlayerParticipantStats['", champ, "'].statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               ]

    --queryFilter         :: QueryColumn model typ -> Value -> Document  -- ^ Produce the document to be used as a filter when given a value.
    queryFilter c@QGameQueueType = simpleFilter "gameStats.queueType" c
    queryFilter c@QGameLength    = simpleFilter "gameStats.gameLength" c
    queryFilter (QGameSummoner  sNameT) = summonerFilter sNameT "._summonerName"
    queryFilter (QGameChampion  sNameT) = summonerFilter sNameT ".skinName"
    queryFilter (QGameWins    sNameT) = summonerFilter sNameT ".statistics.Victories" -- Wins total
    queryFilter (QGameWinPct  sNameT) = undefined -- TODO
    queryFilter (QGameKills   sNameT) = undefined -- TODO
    queryFilter (QGameDeaths  sNameT) = undefined -- TODO
    queryFilter (QGameAssists sNameT) = undefined -- TODO
    queryFilter (QGameKPM     sNameT) = undefined -- TODO
    queryFilter (QGameDPM     sNameT) = undefined -- TODO
    queryFilter (QGameAPM     sNameT) = undefined -- TODO
    queryFilter (QGameGold    sNameT) = undefined -- TODO
    queryFilter (QGameGPM     sNameT) = undefined -- TODO
    queryFilter (QGameCS      sNameT) = undefined -- TODO
    queryFilter (QGameCSPM    sNameT) = undefined -- TODO

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
                              , "(  this.gameStats.otherTeamPlayerParticipantStats['", name, "']"
                              , "&& this.gameStats.otherTeamPlayerParticipantStats['", name, "']", subSelector
                              , ")"
                              ]

summonerFilter :: Text -> UString -> Value -> Document
summonerFilter textName subSelector v = 
        let name = t2u textName
        in [ "$or" =: [ [US.concat ["gameStats.teamPlayerParticipantStats.", name, subSelector] := v]
                      , [US.concat ["gameStats.otherTeamPlayerParticipantStats.", name, subSelector] := v]
                      ]]
