module Model.Game ( module Model.Game
                  , module Data.GameLog
                  ) where

import Prelude
import Yesod
import Data.Text(Text)
import Data.Time
import qualified Data.Map as M
import Text.Printf
import Data.Maybe (fromMaybe)
import Data.GameLog
import Data.GameLog.Persist
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

playerKills :: PlayerStats -> Int
playerKills player = fromMaybe 0 $ lookupStat player "Champion Kills"

playerDeaths :: PlayerStats -> Int
playerDeaths player = fromMaybe 0 $ lookupStat player "Deaths"

playerAssists :: PlayerStats -> Int
playerAssists player = fromMaybe 0 $ lookupStat player "Assists"

playerGold :: PlayerStats -> Int
playerGold player = fromMaybe 0 $ lookupStat player "Gold Earned"

playerMinionsSlain :: PlayerStats -> Int
playerMinionsSlain player = fromMaybe 0 $ lookupStat player "Minions Slain"

playerNeutralMobs :: PlayerStats -> Int
playerNeutralMobs player = fromMaybe 0 $ lookupStat player "Neutral Monsters Killed"

playerCreepScore :: PlayerStats -> Int
playerCreepScore player = playerNeutralMobs player + playerMinionsSlain player

playerVictory :: PlayerStats -> Int
playerVictory player = fromMaybe 0 $ lookupStat player "Victories"

lookupStat :: PlayerStats -> Text -> Maybe Int
lookupStat player stat = do
    let stats = psstatistics player
    value <- M.lookup stat stats
    return $ value

roundLargeNumber :: (Integral i) => i -> String
roundLargeNumber i = if i < 1000
                     then show i
                     else if i < 1000000
                        then printf "%.1fk" (fromIntegral i / 1000 :: Float)
                        else printf "%.1fM" (fromIntegral i / 1000000 :: Float)

formatGameTime :: Int -> String
formatGameTime i = let hours = i `div` 3600
                       min   = (i `div` 60) `mod` 60
                       secs  = i `mod` 60
                    in if hours > 0
                       then printf "%02d:%02d:%02d" hours min secs
                       else printf "%02d:%02d" min secs

playerTeamWon :: M.Map Text PlayerStats -> Bool
playerTeamWon team =
    (> 0) . playerVictory . head . M.elems $ team

