module Handler.Summoner where

import Import

import Settings.StaticFiles

import qualified Data.Text as T
import Model.Game.Query
import Text.Printf

getSummonerStatsR :: String -> Handler RepHtml
getSummonerStatsR name = do
    let champT = T.pack name
        champTableId = "champ-stats-table" :: String
    let columns = [ QGameWinPct   champT
                  , QGameKPM      champT
                  , QGameDPM      champT
                  , QGameAPM      champT
                  , QGameCSPM     champT
                  , QGameGPM      champT
                  ]
    champData <- runDB . runMapReduce $ buildQuery (QGameChampion champT) [] columns
    defaultLayout $ do
        addScript $ StaticR js_jquery_tablesorter_min_js -- for a pretty table.
        setTitle "Game Index"
        $(widgetFile "summoner/view")
    where
        forceString str = (str :: String)
