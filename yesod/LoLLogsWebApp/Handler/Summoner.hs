{-# LANGUAGE ExistentialQuantification #-}
module Handler.Summoner where

import Import

import Model.Game.Query
import Text.Printf
import Data.Text as T (append)
import Data.Maybe (catMaybes)

import Yesod.Widget.Pager
import Yesod.Widget.AjaxFrame

import Model.Champion

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
                    FormSuccess query -> query
                    _                 -> Query (QPlayerChampion summonerName) ["RANKED_SOLO_5x5", "NORMAL"] [summonerName] [] (queryCols summonerName)

    -- DB Calls
    champions  <- championsByName
    dataRows <- runDB $ runQuery query
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


{- The query for data. -}
data Query = Query { qKey       :: QueryColumn Game Text
                   , qQueueTypes:: [Text]
                   , qSummoners :: [Text]
                   , qChampions :: [Text]
                   , qCols      :: [QueryColumn Game Double]
                   }
runQuery query = runMapReduce $ buildQuery 
    (qKey query) -- (QPlayerChampion summonerName) 
    (catMaybes [summonerFilters, championFilters, queueTypeFilters])
    (qCols query)
    where
        summonerFilters  = case qSummoners query of
            [] -> Nothing
            s  -> Just . anyFilter . map (QGameSummoner .==) $ s
        championFilters  = case qChampions query of
            [] -> Nothing
            s  -> Just . anyFilter . map (QGameChampion .==) $ s
        queueTypeFilters = case qQueueTypes query of
            [] -> Nothing
            s  -> Just . anyFilter . map (QGameQueueType .==) $ s

dataForm :: Text -> Html -> MForm LoLLogsWebApp LoLLogsWebApp (FormResult Query, Widget)
dataForm summonerName extra = do
    (queueRes, queueView) <- mreq (multiSelectField queueTypes) "" Nothing
    let q = Query <$> pure (QPlayerChampion summonerName) 
                  <*> queueRes 
                  <*> pure [summonerName] 
                  <*> pure [] 
                  <*> pure (queryCols summonerName)
    let widget = $(widgetFile "summoner/query-form")
    return (q, widget)

queueTypes :: [(Text, Text)]
queueTypes = [("Ranked, Solo", "RANKED_SOLO_5x5"), ("Normal", "NORMAL"), ("Bot", "BOT")]

{-
colsSelect :: [(Text, UString)]
colsSelect = [("Win Percent", "winPct"), ("Kills / Min", "kpm"), ("Deaths / Min", "dpm"),
              ("Assists / Min", "apm"), ("CS / Min", "cspm"), ("Gold / Min", "gpm")]

data RenderType = Chart | Table
    deriving (Show, Eq)

getQuery :: FormResult Query -> (Bool, Query)
getQuery res =
    case res of
        FormSuccess query -> (False, query)
        _                 -> (True, Query ["winPct", "kpm", "dpm", "apm", "cspm", "gpm"] Table)

summonerChart :: ChartInfo
summonerChart =
    setTitles "Summoner Statistics" "" "" $
    setHorizontal True $
    defaultChart

getSeries :: [(UString, Map UString Value)] -> [UString] -> [SeriesInfo String Double]
getSeries dataRows cols = fmap f cols
  where
    f col = SeriesInfo (T.unpack $ prettyName col) (findCol col dataRows)
    prettyName col = fst . head $ Import.filter (\x -> snd x == col) colsSelect


findCol :: UString -> [(UString, Map UString Value)] -> [(String, Double)]
findCol col dat= (catMaybes $ Import.map (\(champ, results) -> ((,) $ US.unpack champ) <$> (M.lookup col results >>= cast')) dat)


prettyMultiSelect :: Widget
prettyMultiSelect = do
        addScript $ StaticR js_jquery_asmselect_js
        addStylesheet $ StaticR css_jquery_asmselect_css
        toWidget [julius|
            $(function() {
                $(".multi select").attr("title", "Please select columns").asmSelect({
                    removeClass: "asmListItemRemove btn danger",
                })
            });
        |]
-}

chosenImports :: Widget
chosenImports = do
    addScript     $ StaticR lib_chosen_chosen_jquery_min_js
    addStylesheet $ StaticR lib_chosen_chosen_css
    toWidget [julius|
        $(function() {
            $("select").chosen();
        });
    |]
