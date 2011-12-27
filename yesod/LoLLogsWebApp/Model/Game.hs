module Model.Game ( module Model.Game
                  , module Data.GameLog
                  ) where

import Prelude
-- import Yesod hiding (Unique, EntityField, PersistEntity, Key, )
import Data.Text as T (Text)
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M
import Text.Printf
import Data.Maybe (fromMaybe)
import Data.GameLog
import Data.GameLog.Persist ()
import Database.Persist.Base
import Database.Persist.MongoDB (Action)
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
        | typ ~ Text      => GameSummoners
        | typ ~ Text      => GameChampions
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
    persistColumnDef GameSummoners  = Database.Persist.Base.ColumnDef "gameStats.summoners" "Text" []
    persistColumnDef GameChampions  = Database.Persist.Base.ColumnDef "gameStats.champions" "Text" []

-- | Return whether the reporting player's team won.
gameBlueTeamWon :: Game -> Bool
gameBlueTeamWon game = 
    let stats = gsPlayerStats . gameGameStats $ game
     in (> 0) . maybe 0 playerVictory . (\s -> M.lookup s stats) . head . gsBlueTeam . gameGameStats $ game

gameLookupPlayer :: Text -> Game -> Maybe PlayerStats
gameLookupPlayer name = M.lookup name . gsPlayerStats . gameGameStats

-- | Get the kill-count for the player.
playerKills :: PlayerStats -> Int
playerKills = fromMaybe 0 . lookupStat "Champion Kills"

playerDeaths :: PlayerStats -> Int
playerDeaths = fromMaybe 0 . lookupStat "Deaths"

playerAssists :: PlayerStats -> Int
playerAssists = fromMaybe 0 . lookupStat "Assists"

playerGold :: PlayerStats -> Int
playerGold = fromMaybe 0 . lookupStat "Gold Earned"

playerMinionsSlain :: PlayerStats -> Int
playerMinionsSlain = fromMaybe 0 . lookupStat "Minions Slain"

playerNeutralMobs :: PlayerStats -> Int
playerNeutralMobs = fromMaybe 0 . lookupStat "Neutral Monsters Killed"

playerCreepScore :: PlayerStats -> Int
playerCreepScore player = playerNeutralMobs player + playerMinionsSlain player

playerVictory :: PlayerStats -> Int
playerVictory = fromMaybe 0 . lookupStat "Victories"

lookupStat :: Text -> PlayerStats -> Maybe Int
lookupStat stat player = do
    let stats = psStatistics player
    value <- M.lookup stat stats
    return $ value

playerTeamWon :: M.Map Text PlayerStats -> Bool
playerTeamWon team =
    (> 0) . playerVictory . head . M.elems $ team


roundLargeNumber :: (Integral i) => i -> String
roundLargeNumber i = if i < 1000
                     then show i
                     else if i < 1000000
                        then printf "%.1fk" (fromIntegral i / 1000 :: Float)
                        else printf "%.1fM" (fromIntegral i / 1000000 :: Float)

formatGameTime :: Int -> String
formatGameTime i = let hours = i `div` 3600
                       mins  = (i `div` 60) `mod` 60
                       secs  = i `mod` 60
                    in if hours > 0
                       then printf "%02d:%02d:%02d" hours mins secs
                       else printf "%02d:%02d" mins secs

gameFormattedCreateTime :: Game -> String
gameFormattedCreateTime = formatTime defaultTimeLocale "%F" . gameCreated

queueDisplayName :: Text -> Text
queueDisplayName "NORMAL"           = "Normal"
queueDisplayName "RANKED_SOLO_5x5"  = "Ranked, Solo"
queueDisplayName "BOT"              = "Bot"
queueDisplayName s                  = s

