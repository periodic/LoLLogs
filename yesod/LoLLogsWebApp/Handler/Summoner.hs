module Handler.Summoner where

import Import

import Model.Game.Query
import Text.Printf
import Data.Text as T (append)

import Yesod.Widget.Pager

import Model.Champion

champPortrait :: Text -> ChampionMap -> Widget
champPortrait skinName champions = $(widgetFile "game/champion-portrait")

portraits :: ChampionMap -> Game -> Widget
portraits champions game = $(widgetFile "game/champions")

getSummonerSearchR :: Handler RepHtml
getSummonerSearchR = do
    summonerName <- runInputGet $ ireq textField "q"
    redirect RedirectPermanent $ SummonerStatsR summonerName

getSummonerStatsR :: Text -> Handler RepHtml
getSummonerStatsR summonerName = do
    -- Parameters for DB calls
    let columns = [ QGameWinPct   summonerName
                  , QGameKPM      summonerName
                  , QGameDPM      summonerName
                  , QGameAPM      summonerName
                  , QGameCSPM     summonerName
                  , QGameGPM      summonerName
                  ]

    -- DB Calls
    champions  <- championsByName
    dataRows <- runDB . runMapReduce $ buildQuery (QGameChampion summonerName) [exists $ QGameSummoner summonerName] columns
    (games, pagerOpts) <- paginateSelectList 10 ([GameTeamPlayerSummoner summonerName ==. summonerName] ||. [GameOtherTeamPlayerSummoner summonerName ==. summonerName]) []

    -- Intermediate data
    let champData = filter ((/= "_total") . fst) dataRows
    let totals    = filter ((== "_total") . fst) dataRows

    -- Widget
    defaultLayout $ do
        let gameList = $(widgetFile "game/list")
        addScript $ StaticR js_jquery_tablesorter_min_js -- for a pretty table.
        champTableId <- lift newIdent
        setTitle . toHtml $ T.append "Stats for " summonerName
        $(widgetFile "summoner/view")

    where
        formatPct :: Double -> String
        formatPct d = printf "%2.1f%%" (d * 100)
        formatDouble :: Double -> String
        formatDouble d = printf "%0.2f" d
