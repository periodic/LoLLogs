module Handler.Chart where

import Import
import Data.List

data ChartInfo = ChartInfo {
    title :: String
  , xAxis :: String
  , yAxis :: String
}

data SeriesInfo k v = SeriesInfo {
    label :: String
  , points :: [(k, v)]
}

noTitleChart :: ChartInfo
noTitleChart = ChartInfo "" "" ""

lineChartSimple :: (Num a) => [(a, a)] -> Widget
lineChartSimple values = lineChart noTitleChart [SeriesInfo "" values]

barChartSimple :: (Num a) => [(String, a)] -> Widget
barChartSimple values = barChart noTitleChart [SeriesInfo "" values]

lineChart :: (Show k, Num v) => ChartInfo -> [SeriesInfo k v] -> Widget
lineChart chart series = callFlot chart dataPoints opts
  where
    dataPoints = renderDataPoints show series
    opts = "{}"
    
barChart :: (Show k, Num v) => ChartInfo -> [SeriesInfo k v] -> Widget
barChart chart series = callFlot chart dataPoints opts
  where
    dataPoints = renderDataPoints showBarYs series
    ticks = renderXTicks . points $ head series
    opts = "{xaxis: {ticks: " ++ ticks ++ "}, series: {bars: {show: true}}}"

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
renderSeries f series = "{label: \"" ++ label series ++ "\", data: " ++ dataPoints ++ "}"
  where
    dataPoints = fmap replaceParens . f $ points series

-- Turns [("Foo", 5), ("Bar", 6)] into "[[0, 5], [1, 6]]"
showBarYs :: (Num v) => [(k, v)] -> String
showBarYs series = fmap replaceParens . show $ showBarYs' (0::Int) series
  where
    showBarYs' i ((_,v):rest) = (i, v) : showBarYs' (i+1) rest
    showBarYs' _ [] = []

-- Needed because flot uses arrays instead of tuples for pairs
replaceParens :: Char -> Char
replaceParens c | c == '(' = '['
                | c == ')' = ']'
                | otherwise = c

callFlot :: ChartInfo -> String -> String -> Widget
callFlot info dataPoints opts = do
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    -- todo use static route for this
    addScriptRemote "/static/js/jquery.flot.min.js"
    -- Needed because flot doesn't work well with bootstrap
    toWidget [lucius|.flot-chart table { width: auto }|]
    containerId <- lift newIdent
    chartId <- lift newIdent
    toWidget [hamlet|
        <span id="#{containerId}" style="display:inline-block">
            <p style="text-align: center"> #{title info}
            <div id="#{chartId}" class="flot-chart" style="width:600px;height:300px;">
            <p style="text-align: center"> #{xAxis info}
    |]
    toWidget [julius|
        $(function() {
            $.plot($('##{chartId}'), [#{dataPoints}], #{opts});
        })
    |]
