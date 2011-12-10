module Handler.Chart where

import Import
import Data.List
import Data.Char

data ChartType = LineChart | BarChart
    deriving (Eq)

data ChartInfo = ChartInfo {
    ciTitle  :: String
  , ciXTitle :: String
  , ciYTitle :: String
  , ciXSize  :: Int
  , ciYSize  :: Int
  , ciHorizontal :: Bool
  , ciType   :: ChartType
}

data SeriesInfo k v = SeriesInfo {
    siLabel :: String
  , siData  :: [(k, v)]
}


defaultChart :: ChartInfo 
defaultChart = ChartInfo "" "" "" 600 300 False BarChart

setType :: ChartType -> ChartInfo -> ChartInfo
setType typ chart = chart{ciType=typ}

setSize :: Int -> Int -> ChartInfo -> ChartInfo
setSize x y chart = chart{ciXSize=x,ciYSize=y}

setHorizontal :: Bool -> ChartInfo -> ChartInfo
setHorizontal b chart = chart{ciHorizontal=b}

setTitles :: String -> String -> String -> ChartInfo -> ChartInfo
setTitles title xTitle yTitle chart = chart{ciTitle=title, ciXTitle=xTitle, ciYTitle=yTitle}

makeChartSimple :: (Show k, Num v) => [(k, v)] -> Widget
makeChartSimple values = makeChart defaultChart [SeriesInfo "" values]

makeChart :: (Show k, Num v) => ChartInfo -> [SeriesInfo k v] -> Widget
makeChart chart | ciType chart == BarChart = barChart chart
                | otherwise = lineChart chart

---

lineChart :: (Show k, Num v) => ChartInfo -> [SeriesInfo k v] -> Widget
lineChart chart series = callFlot chart dataPoints opts
  where
    dataPoints = renderDataPoints show series
    opts = "{}"
    
barChart :: (Show k, Num v) => ChartInfo -> [SeriesInfo k v] -> Widget
barChart chart series = callFlot chart dataPoints opts
  where
    isH = ciHorizontal chart
    dataPoints = renderDataPoints renderFunc series
    renderFunc = if isH then showBarYsFlipped else showBarYs
    ticks = renderXTicks . siData $ head series
    axis = if isH then "yaxis" else "xaxis"
    opts = "{" ++ axis ++ ": {ticks: " ++ ticks ++ "}, series: {bars: {show: true, horizontal: " ++ map toLower (show isH) ++ "}}}"

--- 

-- Turns [("Foo", 5), ("Bar", 6")] into "[(0.5, "Foo"), (1.5, "Bar")]"
renderXTicks :: (Show k) => [(k, v)] -> String
renderXTicks series = fmap replaceParens . show $  getXTicks' 0 series
  where
    getXTicks' i ((c,_):rest) = (i + 0.5 :: Double, c) : getXTicks' (i+1) rest
    getXTicks' _ [] = []

-- Returns string array of multiple series (see renderSeries)
renderDataPoints :: (Show k, Num v) => ([(k,v)] -> String) -> [SeriesInfo k v] -> String
renderDataPoints f series = concat $ intersperse "," $ fmap (renderSeries f) series

-- Turns [SeriesInfo "Foo" [(1,2)]] into "{label: "Foo", data: [1,2]}"
renderSeries :: (Show k, Num v) => ([(k, v)] -> String) -> SeriesInfo k v -> String
renderSeries f series = "{label: \"" ++ siLabel series ++ "\", data: " ++ dataPoints ++ "}"
  where
    dataPoints = fmap replaceParens . f $ siData series

-- Turns [("Foo", 5), ("Bar", 6)] into "[[0, 5], [1, 6]]"
showBarYs :: (Show k, Num v) => [(k, v)] -> String
showBarYs series = fmap replaceParens . show $ showBarYs' (0::Int) series
  where
    showBarYs' i ((_,v):rest) = (i, v) : showBarYs' (i+1) rest
    showBarYs' _ [] = []

-- TODO: Remove duplicate code
showBarYsFlipped :: (Show k, Num v) => [(k, v)] -> String
showBarYsFlipped series = fmap replaceParens . show $ showBarYs' (0::Int) series
  where
    showBarYs' i ((_,v):rest) = (v, i) : showBarYs' (i+1) rest
    showBarYs' _ [] = []

-- Needed because flot uses arrays instead of tuples for pairs
replaceParens :: Char -> Char
replaceParens c | c == '(' = '['
                | c == ')' = ']'
                | otherwise = c

-- TODO: Display Y Axis label; flip labels when horizontal
callFlot :: ChartInfo -> String -> String -> Widget
callFlot info dataPoints opts = do
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    -- TODO: use static route for this
    addScriptRemote "/static/js/jquery.flot.min.js"
    -- Needed because flot doesn't work well with bootstrap
    toWidget [lucius|.flot-chart table { width: auto }|]
    containerId <- lift newIdent
    chartId <- lift newIdent
    toWidget [hamlet|
        <span id="#{containerId}" style="display:inline-block">
            <p style="text-align: center"> #{ciTitle info}
            <div id="#{chartId}" class="flot-chart" style="width:600px;height:300px;">
            <p style="text-align: center"> #{ciXTitle info}
    |]
    toWidget [julius|
        $(function() {
            $.plot($('##{chartId}'), [#{dataPoints}], #{opts});
        })
    |]
