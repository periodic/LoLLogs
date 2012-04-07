module Model.Game ( module Model.Game
                  , module Data.GameLog
                  ) where

import Prelude
-- import Yesod hiding (Unique, EntityField, PersistEntity, Key, )
import Data.Text (Text, isPrefixOf)
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.GameLog hiding (Spell(..))
import Data.GameLog.Persist ()
import Database.Persist
import Database.Persist.Store
import Database.Persist.EntityDef
import Database.Persist.MongoDB (Action)
import Database.Persist.TH.Library

data GameGeneric backend
    = Game {gameCreated :: UTCTime, gameGameStats :: GameStats}
    deriving (Show, Read, Eq)
type Game = GameGeneric Action
type GameId = Key Action Game
instance PersistEntity (GameGeneric backend) where
    data Unique (GameGeneric backend) backend2 = UniqueGameId Int deriving (Show, Read, Eq)
    type PersistEntityBackend (GameGeneric backend) = backend
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
        = EntityDef
            (HaskellName "Game")
            (DBName "Game")
            (DBName "Game")
            []
            [FieldDef (HaskellName "created")   (DBName "created")   (FTTypeCon Nothing "UTCTime")   [],
             FieldDef (HaskellName "gameStats") (DBName "gameStats") (FTTypeCon Nothing "GameStats") []]
            []
            ["Show", "Read", "Eq"]
            M.empty
    toPersistFields (Game created stats)
        = [ SomePersistField created
          , SomePersistField stats]
    fromPersistValues [created, stats]
        = ((Right Game
          `Database.Persist.TH.Library.apE`
            fromPersistValue created)
         `Database.Persist.TH.Library.apE`
           fromPersistValue stats)
    fromPersistValues _ = Left "Invalid fromPersistValues input"
    halfDefined = Game undefined undefined
    persistUniqueToFieldNames UniqueGameId {}    = [(HaskellName "gameStats.gameId", DBName "gameStats.gameId")]
    persistUniqueToValues     (UniqueGameId gid) = [toPersistValue gid]
    persistUniqueKeys (Game _created _gameStats) = []
    {-
    persistColumnDef GameId         = FieldDef (HaskellName "id")                   (DBName "id")                   "GameId" []
    persistColumnDef GameCreated    = FieldDef (HaskellName "created")              (DBName "created")              "UTCTime" []
    persistColumnDef GameGameStats  = FieldDef (HaskellName "gameStats")            (DBName "gameStats")            "GameStats" []
    persistColumnDef GameGameId     = FieldDef (HaskellName "gameStats.gameId")     (DBName "gameStats.gameId")     "Int" []
    persistColumnDef GameRanked     = FieldDef (HaskellName "gameStats.ranked")     (DBName "gameStats.ranked")     "Bool" []
    persistColumnDef GameLength     = FieldDef (HaskellName "gameStats.gameLength") (DBName "gameStats.gameLength") "Int" []
    persistColumnDef GameSummoners  = FieldDef (HaskellName "gameStats.summoners")  (DBName "gameStats.summoners")  "Text" []
    persistColumnDef GameChampions  = FieldDef (HaskellName "gameStats.champions")  (DBName "gameStats.champions")  "Text" []
    -}

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

playerTeamWon :: M.Map Text PlayerStats -> Bool
playerTeamWon team =
    (> 0) . playerVictory . head . M.elems $ team

totalStat :: Text -> [Text] -> Game -> Int
totalStat statName players game =
    let playerStats = map (flip gameLookupPlayer game) players
     in sum . map (fromMaybe 0) . map (maybe (Just 0) (lookupStat statName)) $ playerStats

totalKDA :: [Text] -> Game -> (Int, Int, Int)
totalKDA players game =
    let k = totalStat "Champion Kills" players game
        d = totalStat "Deaths"         players game
        a = totalStat "Assists"        players game
     in (k,d,a)

totalCS :: [Text] -> Game -> Int
totalCS players game =
    let neutral = totalStat "Neutral Monsters Killed" players game
        minions = totalStat "Minions Slain"           players game
     in neutral + minions

totalGold :: [Text] -> Game -> Int
totalGold players game = totalStat "Gold Earned" players game

{- PlayerStats related stuff.
 -}
lookupStat :: Text -> PlayerStats -> Maybe Int
lookupStat stat player = do
    let stats = psStatistics player
    value <- M.lookup stat stats
    return $ value

itemStats :: M.Map Text Int -> M.Map Text Int
itemStats = M.filterWithKey (\k v -> "**ITEM" `isPrefixOf` k)

nonItemStats :: M.Map Text Int -> M.Map Text Int
nonItemStats = M.filterWithKey (\k v -> not $ "**ITEM" `isPrefixOf` k)

{- Formatting.
 -}
queueDisplayName :: Text -> Text
queueDisplayName "NORMAL"           = "Normal"
queueDisplayName "RANKED_SOLO_5x5"  = "Ranked, Solo"
queueDisplayName "BOT"              = "Bot"
queueDisplayName s                  = s

gameFormattedCreateTime :: Game -> String
gameFormattedCreateTime = formatTime defaultTimeLocale "%F" . gameCreated

