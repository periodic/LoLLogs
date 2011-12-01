{-# LANGUAGE Rank2Types, TemplateHaskell, QuasiQuotes, OverloadedStrings, NoImplicitPrelude, CPP, OverloadedStrings, MultiParamTypeClasses, TypeFamilies, GADTs, GeneralizedNewtypeDeriving #-}
module Model.Game (module Model.Game, module Data.GameLog) where

import Prelude
import Yesod
import Data.Time
import Data.GameLog
import Database.Persist.Base
import Database.Persist.MongoDB
import Database.Persist.TH.Library

data GameGeneric backend
    = Game {gameCreated :: UTCTime, gameGameStats :: GameStats}
    deriving (Show, Read, Eq)
type Game = GameGeneric Action
type GameId = Key Action Game
instance PersistEntity (GameGeneric backend) where
    data Unique (GameGeneric backend) backend2 = UniqueGameId Int deriving (Show, Read, Eq)
    data EntityField (GameGeneric backend) typ
        = typ ~ (Key backend (GameGeneric backend)) => GameId 
        | typ ~ UTCTime   => GameCreated
        | typ ~ Bool      => GameRanked
        | typ ~ Int       => GameGameId
        | typ ~ Int       => GameLength
        | typ ~ GameStats => GameGameStats
    entityDef _
        = Database.Persist.Base.EntityDef
            "Game"
            []
            [Database.Persist.Base.ColumnDef "created" "UTCTime" [],
             Database.Persist.Base.ColumnDef "gameStats" "GameStats" []]
            []
            ["Show", "Read", "Eq"]
    toPersistFields (Game created stats)
        = [ Database.Persist.Base.SomePersistField created
          , Database.Persist.Base.SomePersistField stats]
    fromPersistValues [created, stats]
        = ((Right Game
          `Database.Persist.TH.Library.apE`
            fromPersistValue created)
         `Database.Persist.TH.Library.apE`
           fromPersistValue stats)
    fromPersistValues _ = Left "Invalid fromPersistValues input"
    halfDefined = Game undefined undefined
    persistUniqueToFieldNames UniqueGameId {}    = ["gameStats.gameId"]
    persistUniqueToValues     (UniqueGameId gid) = [toPersistValue gid]
    persistUniqueKeys (Game _created _gameStats) = []
    persistColumnDef GameId         = Database.Persist.Base.ColumnDef "id" "GameId" []
    persistColumnDef GameCreated    = Database.Persist.Base.ColumnDef "created" "UTCTime" []
    persistColumnDef GameGameStats  = Database.Persist.Base.ColumnDef "gameStats" "GameStats" []
    persistColumnDef GameGameId     = Database.Persist.Base.ColumnDef "gameStats.gameId" "Int" []
    persistColumnDef GameRanked     = Database.Persist.Base.ColumnDef "gameStats.ranked" "Bool" []
    persistColumnDef GameLength     = Database.Persist.Base.ColumnDef "gameStats.gameLength" "Int" []
