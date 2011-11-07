module LoLStats where

import Game.Log

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

data StatsRow = StatsRow { gameId       :: Integer
                         , queueType    :: String
                         , summonerName :: String
                         , championName :: String
                         , kills        :: Integer
                         , deaths       :: Integer
                         , assists      :: Integer
                         , creepScore   :: Integer
                         , goldEarned   :: Integer
                         , win          :: Integer
                         , duration     :: Integer
                         } deriving (Show, Eq)


getStats :: GameStats -> Maybe StatsRow
getStats game = do
    p       <- getPlayer game
    champ   <- psskinName p
    kills   <- getStat p "CHAMPIONS_KILLED"
    deaths  <- getStat p "NUM_DEATHS"
    assists <- getStat p "ASSISTS"
    creep   <- getStat p "MINIONS_KILLED"
    gold    <- getStat p "GOLD_EARNED"
    let name        = ps_summonerName p
        duration    = gsgameLength  $ game
        gId         = gsgameId      $ game
        queue       = gsqueueType   $ game
        win         = fromMaybe 0 . getStat p $ "WIN"
    return $ StatsRow gId queue name champ kills deaths assists creep gold win duration

toCSV :: StatsRow -> String
toCSV (StatsRow a b c d e f g h i j l) = intercalate "," [show a, show b, show c, show d, show e, show f, show g, show h, show i, show j, show l]

csvHeader :: String
csvHeader = intercalate "," . map (show) $ ["Game ID", "Queue Type", "Summoner", "Champion", "Kills", "Deaths", "Assists", "CreepScore", "Gold", "Win", "Duration"]

getPlayer :: GameStats -> Maybe PlayerStats
getPlayer stats = case Prelude.filter (psisMe) (thisTeam ++ thatTeam) of
                            []      -> Nothing
                            (p:_)   -> Just p
    where
        thisTeam = lsource . gsteamPlayerParticipantStats $ stats
        thatTeam = lsource . gsotherTeamPlayerParticipantStats $ stats


getStat :: PlayerStats -> String -> Maybe Integer
getStat p statType = case Prelude.filter ((== statType) . statstatTypeName) (lsource . psstatistics $ p) of
                            []      -> Nothing
                            (s:_)   -> Just . statvalue $ s

