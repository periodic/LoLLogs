{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Handler.Chart where

import Import
import Data.Map as M

getGameStatsR :: Handler RepHtml
getGameStatsR = do
    games <- runDB $ selectList [] [Asc GameLength]
    let bins = bin binBy10Mins games 0 10
    let dataPoints = fmap (\(len,cBin) -> (show len, length cBin)) bins
    let chart = createBarChart dataPoints
    defaultLayout $ do
        setTitle "Game Len Analysis"
        $(widgetFile "game-len")

-- Should probably move this elsewhere

createBarChart :: [(String, Int)] -> Widget
createBarChart ds = do
    let ys = getYs ds
    let opts = "{xaxis: {ticks: " ++ getXs ds ++ "}, series: {bars: {show: true}}}"
    callFlot ys opts


getYs :: [(String, Int)] -> String
getYs d = fmap replaceParens . show $ getYs' (0::Int)  d
  where
    getYs' i ((_,v):rest) = (i, v) : getYs' (i+1) rest
    getYs' _ [] = []

getXs :: [(String, Int)] -> String
getXs d = fmap replaceParens . show $  getXs' (0::Int) d
  where
    getXs' i ((c,_):rest) = (fromIntegral i + 0.5 :: Double, c) : getXs' (i+1) rest
    getXs' _ [] = []

-- Needed because flot uses arrays instead of tuples for pairs
replaceParens :: Char -> Char
replaceParens c | c == '(' = '['
                | c == ')' = ']'
                | otherwise = c

callFlot :: String -> String -> Widget
callFlot plots opts = do
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    -- todo use static route for this
    addScriptRemote "/static/js/jquery.flot.min.js"
    chartId <- lift newIdent
    toWidget [hamlet|<div id="#{chartId}" style="width:600px;height:300px;">|]
    toWidget [julius|
        $(function() {
            $.plot($('##{chartId}'), [#{plots}], #{opts});
        })
    |]

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
