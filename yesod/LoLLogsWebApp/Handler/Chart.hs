{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Handler.Chart where

import Import
import Graphics.Rendering.Chart.Simple
import Data.Map as M

getGameStatsR :: Handler RepHtml
getGameStatsR = do
    games <- runDB $ selectList [] [Asc GameLength]
    let bins = bin binBy10Mins games 0 10
    _ <- liftIO $ plotPNG file (xs bins) (ys bins)
    defaultLayout $ do
        setTitle "Game Len Analysis"
        $(widgetFile "game-len")

-- Should probably move this elsewhere

file :: String
file = "static/img/chart-generated.png"

xs :: [(Integer, [(a,Game)])] -> [Double]
xs = fmap (fromIntegral . fst)

ys :: [(Integer, [(a,Game)])] -> [Double]
ys = fmap (fromIntegral . length . snd)

getGameLen :: (a, Game) -> Integer
getGameLen = gsgameLength . gameGameStats . snd

binBy10Mins :: (a, Game) -> Integer
binBy10Mins s = 10 * (div (getGameLen s) 600)

-- Bins a list of a's according to binning function f. Assumes
-- a's are already sorted by binning value
bin :: (a -> Integer) -> [a] -> Integer -> Integer -> [(Integer,[a])]
bin f' as' b' step' = assocs $ bin' (M.empty) f' as' b' step'
  where
    -- todo: bin lists are in reverse order
    bin' m f (a:as) b step
        | f a < b+step = bin' (M.insertWith' (++) b [a] m) f as b step
        | otherwise = bin' (M.insertWith' (++) b [] m) f (a:as) (b+step) step
    bin' m _ _ _ _ = m
