module Handler.ChartExample where

import Import
import Data.Map as M
import Handler.Chart

getGameStatsR :: Handler RepHtml
getGameStatsR = do
    games <- runDB $ selectList [] [Asc GameLength]
    let bins = bin binBy10Mins games 0 10
    let dataPoints = fmap (\(len,cBin) -> (show len, length cBin)) bins
    let chart = barChartSimple dataPoints
    let chart2 = sampleLineChart
    defaultLayout $ do
        setTitle "Game Len Analysis"
        $(widgetFile "game-len")

sampleLineChart :: Widget
sampleLineChart = lineChart (ChartInfo "Title" "XAxis" "YAxis")
    [ SeriesInfo "Series 1" [(1.5,2) :: (Double, Double), (2,3), (3,5), (4,6)]
    , SeriesInfo "Series 2" [(2,3), (4,8), (6, 2)]
    ]


getGameLen :: (a, Game) -> Int
getGameLen = gsgameLength . gameGameStats . snd

binBy10Mins :: (a, Game) -> Int
binBy10Mins s = 10 * (div (getGameLen s) 600)

-- Bins a list of a's according to binning function f. Assumes
-- a's are already sorted by binning value
bin :: (a -> Int) -> [a] -> Int -> Int -> [(Int,[a])]
bin f' as' b' step' = assocs $ bin' (M.empty) f' as' b' step'
  where
    -- todo: bin lists are in reverse order
    bin' m f (a:as) b step
        | f a < b+step = bin' (M.insertWith' (++) b [a] m) f as b step
        | otherwise = bin' (M.insertWith' (++) b [] m) f (a:as) (b+step) step
    bin' m _ _ _ _ = m
