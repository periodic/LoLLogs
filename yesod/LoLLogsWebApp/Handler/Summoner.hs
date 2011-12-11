module Handler.Summoner where

import Import

import Settings.StaticFiles

import Model.Game.Query
import Text.Printf

getSummonerSearchR :: Handler RepHtml
getSummonerSearchR = do
    summonerName <- runInputGet $ ireq textField "q"
    getSummonerStatsR summonerName

getSummonerStatsR :: Text -> Handler RepHtml
getSummonerStatsR summonerName = do
    let columns = [ QGameWinPct   summonerName
                  , QGameKPM      summonerName
                  , QGameDPM      summonerName
                  , QGameAPM      summonerName
                  , QGameCSPM     summonerName
                  , QGameGPM      summonerName
                  ]
    dataRows <- runDB . runMapReduce $ buildQuery (QGameChampion summonerName) [exists $ QGameSummoner summonerName] columns
    let champData = filter ((/= "_total") . fst) dataRows
    let totals    = filter ((== "_total") . fst) dataRows
    defaultLayout $ do
        addScript $ StaticR js_jquery_tablesorter_min_js -- for a pretty table.
        champTableId <- lift newIdent
        setTitle "Game Index"
        $(widgetFile "summoner/view")
    where
        forceString str = (str :: String)
