module Handler.ChartExample where

import Import
import Data.Map as M
import Model.Game.Query
import Handler.Chart
import Data.Maybe (catMaybes)

-- TODO: This really shouldn't be necessary and the BSON manipulation should be moved out to the Helper.
import Data.Bson
import Data.UString as US (unpack)

getGameStatsR :: Handler RepHtml
getGameStatsR = do
    games <- runDB $ selectList [] [Asc GameLength]
    let bins = bin binBy10Mins games 0 10
    let dataPoints = fmap (\(len,cBin) -> (show len, length cBin)) bins
    let chart = makeChartSimple dataPoints

    queryData <- runDB . runMapReduce $ buildQuery (QGameChampion "ShaperOfChaos") [] [QGameWinPct "ShaperOfChaos"]
    let dataPoints2 = catMaybes $ Import.map (\(champ, results) -> ((,) $ US.unpack champ) <$> (M.lookup "winPct" results >>= cast')) queryData

    let chart2 = makeChart summonerChart [SeriesInfo "Game Length" (dataPoints2 :: [(String, Double)])]

    defaultLayout $ do
        setTitle "Game Len Analysis"
        $(widgetFile "game-len")

summonerChart :: ChartInfo
summonerChart = 
    setTitles "Game Length by Summoner" "" "" $
    setHorizontal True $
    defaultChart

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
