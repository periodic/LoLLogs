{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Model.Game.Query ( module Model.Game.Query
                        , module Model.Helper.MapReduce
                        ) where

import Prelude
import Data.Bson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.UString as US

import Model.Game
import Model.Helper.MapReduce

{- The query for data. -}
data Query = Query { qKey       :: QueryColumn Game Text
                   , qQueueTypes:: [Text]
                   , qSummoners :: [Text]
                   , qChampions :: [Text]
                   , qCols      :: [QueryColumn Game Double]
                   }

runQuery query = mapReduce (qKey query)
                           (catMaybes [summonerFilters, championFilters, queueTypeFilters])
                           (qCols query)
    where
        summonerFilters  = case qSummoners query of
            [] -> Nothing
            s  -> Just $ QGameSummoner .<- s
        championFilters  = case qChampions query of
            [] -> Nothing
            s  -> Just $ QGameChampion .<- s
        queueTypeFilters = case qQueueTypes query of
            [] -> Nothing
            s  -> Just $ QGameQueueType .<- s

instance Queryable Game where
    data QueryColumn Game typ
        = typ ~ Text    => QGameQueueType           -- game type, as a string
        | typ ~ Int     => QGameLength              -- game length in seconds.
        | typ ~ Text    => QGameSummoner            -- Summoner Name
        | typ ~ Text    => QGameChampion            -- Champion
        | typ ~ Text    => QPlayerChampion Text -- Champion name, used for filtering and as a key mostly.
        | typ ~ Int     => QPlayerWins Text     -- Wins total
        | typ ~ Double  => QPlayerWinPct Text   -- % wins
        | typ ~ Int     => QPlayerKills Text    -- Total kills
        | typ ~ Int     => QPlayerDeaths Text   -- Total deaths
        | typ ~ Int     => QPlayerAssists Text  -- Total Assists
        | typ ~ Double  => QPlayerKPM Text      -- Kills/min
        | typ ~ Double  => QPlayerDPM Text      -- Deaths/min
        | typ ~ Double  => QPlayerAPM Text      -- Assists/min
        | typ ~ Int     => QPlayerCS Text       -- Creep score (neutral + minions)
        | typ ~ Double  => QPlayerCSPM Text     -- CS/min
        | typ ~ Int     => QPlayerGold Text     -- Gold
        | typ ~ Double  => QPlayerGPM Text      -- Gold / minute

    -- querySelector       :: QueryColumn model typ -> Javascript -- ^ Used to select when used as a key.
    queryKeyCode c@QGameQueueType = simpleKey "gameStats.queueType" c
    queryKeyCode c@QGameLength    = simpleKey "gameStats.gameLength" c
    queryKeyCode c@QGameSummoner  = simpleKey "gameStats.summoners" c
    queryKeyCode c@QGameChampion  = simpleKey "gameStats.champions" c
    queryKeyCode (QPlayerChampion sNameT) = summonerKeyCode sNameT ".skinName"
    queryKeyCode (QPlayerWins    sNameT) = summonerKeyCode sNameT ".statistics.Victories" -- Wins total
    queryKeyCode (QPlayerWinPct  sNameT) = summonerKeyCode sNameT ".statistics.Victories" -- % wins
    queryKeyCode (QPlayerKills   sNameT) = summonerKeyCode sNameT ".statistics['Champion Kills']" -- Total kills
    queryKeyCode (QPlayerDeaths  sNameT) = summonerKeyCode sNameT ".statistics.Deaths" -- Total deaths
    queryKeyCode (QPlayerAssists sNameT) = summonerKeyCode sNameT ".statistics.Assists" -- Total Assists
    queryKeyCode (QPlayerKPM     sNameT) = summonerKeyCode sNameT ".statistics['Champion Kills'] * 60 / this.gameStats.gameLength" -- Kills/min
    queryKeyCode (QPlayerDPM     sNameT) = summonerKeyCode sNameT ".statistics.Deaths * 60 / this.gameStats.gameLength" -- Deaths/min
    queryKeyCode (QPlayerAPM     sNameT) = summonerKeyCode sNameT ".statistics.Assists * 60 / this.gameStats.gameLength" -- Assists/min
    queryKeyCode (QPlayerGold    sNameT) = summonerKeyCode sNameT ".statistics['Gold Earned']" -- Gold
    queryKeyCode (QPlayerGPM     sNameT) = summonerKeyCode sNameT ".statistics['Gold Earned'] * 60 / this.gameStats.gameLength" -- Gold / minute
    queryKeyCode (QPlayerCS      sNameT) = -- Creep score (neutral + minions)
        let champ = t2u sNameT
         in wrapJS $ US.concat [ "(this.gameStats.playerStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.playerStats.", champ, ".statistics['Neutral Monsters Killed'])"
                               ]
    queryKeyCode (QPlayerCSPM    sNameT) = -- CS/min
        let champ = t2u sNameT
         in wrapJS $ US.concat [ "(this.gameStats.playerStats.", champ, ".statistics['Minions Slain']"
                               , "+ this.gameStats.playerStats.", champ, ".statistics['Neutral Monsters Killed']) * 60 / this.gameStats.gameLength"
                               ]

    -- queryColumnName     :: QueryColumn model typ -> UString    -- ^ The column name 
    queryColumnName QGameQueueType     = "queueType"
    queryColumnName QGameLength        = "gameLength"
    queryColumnName QGameSummoner      = "summoner"
    queryColumnName QGameChampion      = "champion" -- TODO: Change this?
    queryColumnName (QPlayerChampion  _) = "champion"
    queryColumnName (QPlayerWins      _) = "wins"
    queryColumnName (QPlayerWinPct    _) = "winPct"
    queryColumnName (QPlayerKills     _) = "kills"
    queryColumnName (QPlayerDeaths    _) = "deaths"
    queryColumnName (QPlayerAssists   _) = "assists"
    queryColumnName (QPlayerKPM       _) = "kpm"
    queryColumnName (QPlayerDPM       _) = "dpm"
    queryColumnName (QPlayerAPM       _) = "apm"
    queryColumnName (QPlayerGold      _) = "gold"
    queryColumnName (QPlayerGPM       _) = "gpm"
    queryColumnName (QPlayerCS        _) = "cs"
    queryColumnName (QPlayerCSPM      _) = "cspm"

    -- queryMapCode        :: QueryColumn model typ -> Javascript -- ^ Should set fields on "result" from "this".  Run once for each document.
    queryMapCode c@QGameQueueType = simpleMap "gameStats.queueType" c
    queryMapCode c@QGameLength    = simpleMap "gameStats.gameLength" c
    queryMapCode c@QGameSummoner  = simpleMap "gameStats.summoners" c
    queryMapCode c@QGameChampion  = simpleMap "gameStats.champions" c
    queryMapCode c@(QPlayerChampion  sNameT) = summonerMapCode sNameT (queryColumnName c) ".skinName"
    queryMapCode c@(QPlayerWins    sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Victories" -- Wins total
    queryMapCode c@(QPlayerWinPct  sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Victories" -- % wins
    queryMapCode c@(QPlayerKills   sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Champion Kills']" -- Total kills
    queryMapCode c@(QPlayerDeaths  sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Deaths" -- Total deaths
    queryMapCode c@(QPlayerAssists sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Assists" -- Total Assists
    queryMapCode c@(QPlayerKPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Champion Kills'] * 60 / this.gameStats.gameLength" -- Kills/min
    queryMapCode c@(QPlayerDPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Deaths * 60 / this.gameStats.gameLength" -- Deaths/min
    queryMapCode c@(QPlayerAPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics.Assists * 60 / this.gameStats.gameLength" -- Assists/min
    queryMapCode c@(QPlayerGold    sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Gold Earned']" -- Gold
    queryMapCode c@(QPlayerGPM     sNameT) = summonerMapCode sNameT (queryColumnName c) ".statistics['Gold Earned'] * 60 / this.gameStats.gameLength" -- Gold / minute
    queryMapCode c@(QPlayerCS sNameT) = -- Creep score (neutral + minions)
        let champ = t2u sNameT
            field = queryColumnName c
         in wrapJS $ US.concat [ "result.", field, " = "
                               , "(  this.gameStats.playerStats['", champ, "']"
                               , "&& (this.gameStats.playerStats['", champ, "'].statistics['Minions Slain']"
                               ,     "+ this.gameStats.playerStats['", champ, "'].statistics['Neutral Monsters Killed']"
                               ,     ")"
                               , ")"
                               ]
    queryMapCode c@(QPlayerCSPM sNameT) = -- CS/min
        let champ = t2u sNameT
            field = queryColumnName c
         in wrapJS $ US.concat [ "result.", field, " = "
                               , "(  this.gameStats.playerStats['", champ, "']"
                               , "&& ("
                               ,       "( this.gameStats.playerStats['", champ, "'].statistics['Minions Slain']"
                               ,       "+ this.gameStats.playerStats['", champ, "'].statistics['Neutral Monsters Killed']"
                               ,       ")"
                               ,     "* 60 / this.gameStats.gameLength"
                               ,     ")"
                               , ")"
                               ]

    --queryFilter         :: QueryColumn model typ -> Value -> Document  -- ^ Produce the document to be used as a filter when given a value.
    queryFilter c@QGameQueueType = simpleFilter "gameStats.queueType" c
    queryFilter c@QGameLength    = simpleFilter "gameStats.gameLength" c
    queryFilter c@QGameSummoner  = simpleFilter "gameStats.summoners" c
    queryFilter c@QGameChampion  = simpleFilter "gameStats.champions" c
    queryFilter (QPlayerChampion sNameT) = summonerFilter sNameT ".skinName"
    queryFilter (QPlayerWins     sNameT) = summonerFilter sNameT ".statistics.Victories" -- Wins total
    queryFilter _               = undefined -- TODO

    -- queryFinalizeCode   :: QueryColumn model typ -> Javascript -- ^ Used to finalize the result.  Should work on "result" and "v".  The value assigned
    -- queryFinalizeCode c@(QGameChampion t)   = simpleFinalize c
    queryFinalizeCode c                     = simpleFinalizeAvg c


summonerKeyCode :: Text -> UString -> Javascript
summonerKeyCode textName subSelector = 
        let name = t2u textName
         in wrapJS $ US.concat [ "(  this.gameStats.playerStats['", name, "']"
                               , "&& this.gameStats.playerStats['", name, "']", subSelector
                               , ")"
                               ]

summonerMapCode :: Text -> UString -> UString -> Javascript
summonerMapCode textName field subSelector = 
        let name = t2u textName
        in wrapJS $ US.concat [ "result.", field, " = "
                              , "(  this.gameStats.playerStats['", name, "']"
                              , "&& this.gameStats.playerStats['", name, "']", subSelector
                              , ")"
                              ]

summonerFilter :: Text -> UString -> Value -> Document
summonerFilter textName subSelector v = 
        let name = t2u textName
        in [US.concat ["gameStats.playerStats.", name, subSelector] := v]

