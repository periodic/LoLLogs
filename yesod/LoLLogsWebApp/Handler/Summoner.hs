module Handler.Summoner where

import Import

import Model.Game.Query
import Text.Printf
import Data.Text as T (append)

import Yesod.Widget.Flot
import Yesod.Widget.Pager

import Model.Champion

import Data.Maybe (catMaybes)
import Data.Bson
import Data.UString as US (unpack)
import Data.Map as M hiding (null)

champPortrait :: Text -> ChampionMap -> Widget
champPortrait skinName champions = $(widgetFile "game/champion-portrait")

portraits :: ChampionMap -> Game -> Widget
portraits champions game = $(widgetFile "game/champions")

data Query = Query
  { qCols :: [UString]
  , qType :: RenderType
  } deriving (Show, Eq)

colsSelect :: [(Text, UString)]
colsSelect = [("Win Percent", "winPct"), ("Kills / Min", "kpm"), ("Deaths / Min", "dpm"),
              ("Assists / Min", "apm"), ("CS / Min", "cspm"), ("Gold / Min", "gpm")]

data RenderType = Chart | Table
    deriving (Show, Eq)

getQuery :: FormResult Query -> Query
getQuery res =
    case res of
        FormSuccess query -> query
        _ -> Query ["winPct", "kpm", "dpm", "apm", "cspm", "gpm"] Table

summonerForm :: Html -> MForm LoLLogsWebApp LoLLogsWebApp (FormResult Query, Widget)
summonerForm extra = do
    let types = [("Chart", Chart), ("Table", Table)]
    (colsRes, colsView) <- mreq (multiSelectField colsSelect) "" Nothing
    (typeRes, typeView) <- mreq (selectField types) "" (Just Table)
    let q = Query <$> colsRes <*> typeRes
    let widget = do
        -- Needed because of bootstrap
        toWidget [lucius|
            .summoner-form label { float: none;}
        |]
        $(widgetFile "summoner/query-form")
    return (q, widget)

summonerChart :: ChartInfo
summonerChart =
    setTitles "Summoner Statistics" "" "" $
    setHorizontal True $
    defaultChart

getSeries :: [(UString, Map UString Value)] -> [UString] -> [SeriesInfo String Double]
getSeries dataRows cols = fmap f cols
  where f col = SeriesInfo (US.unpack col) (findCol col dataRows)

findCol :: UString -> [(UString, Map UString Value)] -> [(String, Double)]
findCol col dat= (catMaybes $ Import.map (\(champ, results) -> ((,) $ US.unpack champ) <$> (M.lookup col results >>= cast')) dat)


queryCols summonerName =
    [("winPct", QGameWinPct summonerName)
    ,("kpm", QGameKPM summonerName)
    ,("dpm", QGameDPM summonerName)
    ,("apm", QGameAPM summonerName)
    ,("cspm", QGameCSPM summonerName)
    ,("gpm", QGameGPM summonerName)
    ]

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

    -- Form Data
    ((res, widget), enctype) <- runFormGet summonerForm
    let query = getQuery res
    let colNames = qCols query
    let cols = fmap snd $ Import.filter (\(a,_) -> a `elem` colNames) $ queryCols summonerName

    -- DB Calls
    champions  <- championsByName
    dataRows <- runDB . runMapReduce $ buildQuery (QGameChampion summonerName) [exists $ QGameSummoner summonerName] cols
    (games, pagerOpts) <- paginateSelectList 10 ([GameTeamPlayerSummoner summonerName ==. summonerName] ||. [GameOtherTeamPlayerSummoner summonerName ==. summonerName]) []


    -- Intermediate data
    let champData = Import.filter ((/= "_total") . fst) dataRows
    let totals    = Import.filter ((== "_total") . fst) dataRows

    let series = getSeries champData colNames
    -- Widget
    defaultLayout $ do
        let gameList = $(widgetFile "game/list")
        addScript $ StaticR js_jquery_tablesorter_min_js -- for a pretty table.
        champTableId <- lift newIdent
        setTitle . toHtml $ T.append "Stats for " summonerName
        prettyMultiSelect
        let stats = if qType query == Table then $(widgetFile "summoner/stats") else makeChart summonerChart series
        $(widgetFile "summoner/view")

    where
        formatPct :: Double -> String
        formatPct d = printf "%2.1f%%" (d * 100)
        formatDouble :: Double -> String
        formatDouble d = printf "%0.2f" d

prettyMultiSelect :: Widget
prettyMultiSelect = do
        addScript $ StaticR js_jquery_asmselect_js
        addStylesheet $ StaticR css_jquery_asmselect_css
        toWidget [julius|
            $(function() {
                $(".multi select").attr("title", "Please select columns").asmSelect()
            });
        |]
    
