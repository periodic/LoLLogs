{-# LANGUAGE ExistentialQuantification #-}
module Handler.Summoner where

import Import

import Data.Text as T (append, pack)
import Text.Printf

import Yesod.Widget.Pager
import Yesod.Widget.AjaxFrame

import Model.Champion
import Model.Game.Query

champPortrait :: Text -> ChampionMap -> Widget
champPortrait skinName champions = $(widgetFile "game/champion-portrait")

portraits :: ChampionMap -> Game -> Widget
portraits champions game = $(widgetFile "game/champions")

queryCols :: Text -> [QueryColumn Game Double]
queryCols summonerName =
    [ QPlayerWinPct summonerName
    , QPlayerKPM summonerName
    , QPlayerDPM summonerName
    , QPlayerAPM summonerName
    , QPlayerCSPM summonerName
    , QPlayerGPM summonerName
    ] 

getSummonerSearchR :: Handler RepHtml
getSummonerSearchR = do
    summonerName <- runInputGet $ ireq textField "q"
    -- $(logDebug) $ "Redirecting query for " `T.append` summonerName
    redirect RedirectPermanent $ SummonerStatsR summonerName

getSummonerStatsR :: Text -> Handler RepHtml
getSummonerStatsR summonerName = do
    let columns = queryCols summonerName

    -- Form Data
    ((res, widget), enctype) <- runFormGet $ dataForm summonerName
    {-
    let (isDefault, query) = getQuery res
    let colNames = qCols query
    let cols = Import.filter (\c -> queryColumnName c `elem` colNames) $ queryCols summonerName
    -}
    let query = case res of
                    FormSuccess qData -> qData
                    _                 -> Query (QPlayerChampion summonerName) ["RANKED_SOLO_5x5", "NORMAL"] [summonerName] [] (queryCols summonerName)

    $(logDebug) . T.pack $ "Queues : " ++ (show $ qQueueTypes query)
    -- DB Calls
    champions          <- championsByName
    dataRows           <- runDB $ runQuery query
    $(logDebug) . T.pack $ "Retrived " ++ (show $ length dataRows) ++ " data rows."
    (games, pagerOpts) <- paginateSelectList 10 [GameSummoners ==. summonerName] []

    -- Intermediate data
    let champData = Import.filter ((/= "_total") . fst) dataRows
    let totals    = Import.filter ((== "_total") . fst) dataRows

    -- let series = getSeries champData colNames

    -- Widget
    defaultLayout $ do
        -- Widgets
        let gameList = $(widgetFile "game/list")
        -- Scripts
        addScript $ StaticR js_jquery_tablesorter_min_js -- for a pretty table.
        chosenImports
        --prettyMultiSelect -- JS for AMS select

        champTableId <- lift newIdent
        setTitle . toHtml $ T.append "Stats for " summonerName

        --let stats = if qType query == Table then $(widgetFile "summoner/stats") else makeChart summonerChart series
        let stats = $(widgetFile "summoner/stats")
        $(widgetFile "summoner/view")

    where
        formatPct :: Double -> String
        formatPct d = printf "%2.1f%%" (d * 100)
        formatDouble :: Double -> String
        formatDouble d = printf "%0.2f" d


dataForm :: Text -> Html -> MForm LoLLogsWebApp LoLLogsWebApp (FormResult Query, Widget)
dataForm summonerName extra = do
    (queueRes, queueView) <- mreq (multiSelectField queueTypes) "" (Just ["RANKED_SOLO_5x5", "NORMAL"])
    let q = Query <$> pure (QPlayerChampion summonerName) 
                  <*> queueRes 
                  <*> pure [summonerName] 
                  <*> pure [] 
                  <*> pure (queryCols summonerName)
    let widget = $(widgetFile "summoner/query-form")
    return (q, widget)

queueTypes :: [(Text, Text)]
queueTypes = map (\x -> (queueDisplayName x, x)) ["RANKED_SOLO_5x5", "NORMAL", "BOT"]

chosenImports :: Widget
chosenImports = do
    addScript     $ StaticR lib_chosen_chosen_jquery_min_js
    addStylesheet $ StaticR lib_chosen_chosen_css
    toWidget [julius|
        $(function() {
            $("select").chosen();
        });
    |]

