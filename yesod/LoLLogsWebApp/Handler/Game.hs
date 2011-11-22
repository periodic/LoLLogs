module Handler.Game where

import Import
import Data.GameLog

getGameIndexR :: Handler RepHtml
getGameIndexR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Game Index"
        $(widgetFile "game-index")
