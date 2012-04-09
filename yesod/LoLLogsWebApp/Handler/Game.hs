module Handler.Game where

import Import

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe, catMaybes)
import Data.Aeson
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)

import Yesod.Widget.AjaxFrame
import Yesod.Widget.Pager

import Model.Champion
import Model.Item
import Model.Spell

import Widget.GameList

gridStats :: [(Text, [Text])]
gridStats = [ ("Combat", [ "Champion Kills"
                         , "Deaths"
                         , "Assists"
                         , "Largest Killing Spree"
                         , "Largest Multi Kill"
                         ])
            , ("Damage Done", [ "Damage Dealt"
                              , "Physical Damage Dealt"
                              , "Magic Damage Dealt"
                              , "Largest Critical Strike"
                              ])
            , ("Damage Taken & Healed", [ "Healing Done"
                                        , "Damage Taken"
                                        , "Physical Damage Taken"
                                        , "Magic Damage Taken"
                                        ])
            , ("Misc.", [ "Gold Earned"
                        , "Turrets Destroyed"
                        , "Inhibitors Destroyed"
                        , "Minions Slain"
                        , "Neutral Monsters Killed"
                        , "Time Spent Dead"
                        ])
            ]

getGameIndexR :: Handler RepHtml
getGameIndexR = do
    --gameData <- runDB $ selectList [] []
    (games, pagerOpts) <- paginateSelectList 10 [] [Desc GameCreated]
    champions <- championsByName
    defaultLayout $ do
        let gamesWidget = gameList Nothing champions games (pagerOpts { pageContext = Just 5 })
        setTitle "Game Index"
        $(widgetFile "game/index")


getGameViewR :: GameId -> Handler RepHtml
getGameViewR gameId = do
    game      <- runDB $ get404 gameId
    champions <- championsByName
    spells    <- spellsById
    items     <- itemsById

    let stats = gameGameStats game

    defaultLayout $ do
        setTitle "Game"
        addScript $ StaticR js_bootstrap_bootstrap_twipsy_js
        addScript $ StaticR js_bootstrap_bootstrap_popover_js
        -- addScript $ StaticR js_bootstrap_bootstrap_tabs_js
        addScript $ StaticR js_jqueryui_jquery_ui_core_min_js
        addScript $ StaticR js_jqueryui_jquery_ui_widget_min_js
        addScript $ StaticR js_jqueryui_jquery_ui_tabs_min_js
        addScript $ StaticR js_jqplot_jquery_jqplot_min_js
        addScript $ StaticR js_jqplot_jqplot_barRenderer_min_js
        addScript $ StaticR js_jqplot_jqplot_categoryAxisRenderer_min_js
        addStylesheet $ StaticR css_jquery_jqplot_css
        $(widgetFile "game/view")
    where
        playerDetails player champions spells items = $(widgetFile "game/player-details")
        -- TODO: This whole thing with the JSON seems a little convoluted...
        statNames = concatMap snd gridStats
        players game = gsBlueTeam (gameGameStats game) ++ gsPurpleTeam (gameGameStats game)
        jsonPlayers = decodeUtf8 . BS.concat . L.toChunks . encode . reverse . players
        statData game = 
            let playerData p = map (\s -> (s, fromMaybe 0 $ lookupStat s p)) statNames
                playerList = players game
             in zip playerList . catMaybes $ map (\p -> playerData <$> gameLookupPlayer p game) playerList
        statDataJson game =
            let notJson = statData game
                fromList :: ToJSON a => [(Text, a)] -> Value
                fromList = Data.Aeson.object . map (\(k,v) -> k .= v)
             in decodeUtf8 . BS.concat . L.toChunks . encode . fromList . map (\(player, stats) -> (player, fromList stats)) $ notJson
        empties itemList = replicate (6 - length itemList) ()


postGameCreateR :: Handler RepJson
postGameCreateR = do
    parsedBody <- parseJsonBody
    case (parsedBody :: Result [GameStats]) of
        (Success games)   -> insertGames games
        (Error msg)       -> return . RepJson . toContent . encode . String . pack $ msg
        --Left msg                -> return . RepJson . toContent . encode . String . pack $ msg
    where
        insertGames games = do
            time <- liftIO getCurrentTime
            mapM_ (insertGame . Game time) games
            return . RepJson . toContent . encode . String . pack $ "Found " ++ show (length games) ++ " games."
        insertGame game = do
            let gameId = gsGameId . gameGameStats $ game
            mGame <- runDB . getBy . UniqueGameId $ fromIntegral gameId
            case mGame of 
                Just _  -> return ()
                Nothing -> (runDB $ insert game) >> return ()
