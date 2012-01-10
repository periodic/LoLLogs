{-# LANGUAGE ExistentialQuantification #-}
module Handler.Summoner where

import Import

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text as T (append, pack)
import Text.Printf

import Yesod.Widget.Pager
import Yesod.Widget.AjaxFrame

import Model.Champion
import Model.Game.Query hiding (Query(..), runQuery)

import Widget.GameList

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
    champions <- championsByName

    -- Widgets
    statsWidget <- statsPane summonerName champions
    gamesWidget <- gamesPane summonerName champions

    defaultLayout $ do
        setTitle . toHtml $ T.append "Stats for " summonerName
        --addScript $ StaticR js_bootstrap_bootstrap_tabs_js
        addScript $ StaticR js_jqueryui_jquery_ui_core_min_js
        addScript $ StaticR js_jqueryui_jquery_ui_widget_min_js
        addScript $ StaticR js_jqueryui_jquery_ui_tabs_min_js
        $(widgetFile "summoner/view")

gamesPane :: Text -> ChampionMap -> Handler Widget
gamesPane summonerName champions = do
    (games, pagerOpts) <- paginateSelectList 10 [GameSummoners ==. summonerName] [Desc GameCreated]

    let gamesWidget = gameList (Just summonerName) champions games pagerOpts

    return $ ajaxFrame defaultFrameOptions gamesWidget



statsPane :: Text -> ChampionMap -> Handler Widget
statsPane summonerName champions = do
    let columns = queryCols summonerName
    -- ID for table container
    statsTableId <- newIdent

    -- Form Data
    ((res, queryForm), enctype) <- runFormGet $ dataForm summonerName champions
    let query = case res of
                    FormSuccess qData -> qData
                    _                 -> Query (QPlayerChampion summonerName) summonerName ["RANKED_SOLO_5x5", "NORMAL"] [] (queryCols summonerName)

    $(logDebug) . T.pack . show $ qQueueTypes query
    $(logDebug) . T.pack . show $ mrFromQuery query

    dataRows <- runDB $ runQuery query
    let champData = Import.filter ((/= "_total") . fst) dataRows
    let totals    = Import.filter ((== "_total") . fst) dataRows


    return $ do
        -- Static scripts
        addScript $ StaticR js_jquery_tablesorter_min_js -- for a pretty table.
        addScript $ StaticR js_bootstrap_bootstrap_buttons_js
        chosenImports
        $(widgetFile "summoner/stats")
    where
        formatPct :: Double -> String
        formatPct d = printf "%2.1f%%" (d * 100)
        formatDouble :: Double -> String
        formatDouble d = printf "%0.2f" d



dataForm :: Text -> ChampionMap -> Html -> MForm LoLLogsWebApp LoLLogsWebApp (FormResult Query, Widget)
dataForm summonerName championMap extra = do
    let queueDefault = ["RANKED_SOLO_5x5", "NORMAL"]
    let champDefault = []
    (queueRes, queueSelect) <- mopt (multiSelectField queueTypes) "" (Just $ Just queueDefault)
    (champRes, champSelect) <- mopt (multiSelectField . map (\(n,c) -> (championName c, n)) . champsAsList $ championMap) "" (Just $ Just champDefault)
    let q = Query <$> pure (QPlayerChampion summonerName) 
                  <*> pure summonerName
                  <*> (fromMaybe [] <$> queueRes)
                  <*> (fromMaybe [] <$> champRes)
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
            $(".noflash").show();
            $("select").chosen();
        });
    |]

{- The query for data. -}
data Query = Query { qKey       :: QueryColumn Game Text
                   , qSummoner  :: Text
                   , qQueueTypes:: [Text]
                   , qChampions :: [Text]
                   , qCols      :: [QueryColumn Game Double]
                   }

runQuery :: Query -> MRBackend (GGHandler LoLLogsWebApp LoLLogsWebApp IO) [(Label, MRData)]
runQuery = runMapReduce . mrFromQuery

mrFromQuery :: Query -> MapReduce
mrFromQuery query = buildQuery (qKey query)
                               (summonerFilter : (catMaybes [championFilters, queueTypeFilters]))
                               (qCols query)
    where
        summonerFilter   = QGameSummoner .== qSummoner query
        championFilters  = case qChampions query of
            [] -> Nothing
            s  -> Just $ QPlayerChampion (qSummoner query) .<- s
        queueTypeFilters = case qQueueTypes query of
            [] -> Nothing
            s  -> Just $ QGameQueueType .<- s
