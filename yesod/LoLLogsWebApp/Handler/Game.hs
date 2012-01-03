module Handler.Game where

import Import

import Data.Enumerator.List (consume)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, catMaybes)
import Data.Aeson (json, Result(..), encode, Value(..))
import Data.Attoparsec (parseOnly)
import Data.Text (pack)
import Data.Time (getCurrentTime)

import Yesod.Widget.AjaxFrame
import Yesod.Widget.Pager

import Model.Champion

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

portraits :: ChampionMap -> Game -> Widget
portraits champions game = $(widgetFile "game/champions")

getGameIndexR :: Handler RepHtml
getGameIndexR = do
    --gameData <- runDB $ selectList [] []
    (games, pagerOpts) <- paginateSelectList 10 [] [Desc GameCreated]
    champions  <- championsByName
    defaultLayout $ do
        let gameList = $(widgetFile "game/list")
        setTitle "Game Index"
        $(widgetFile "game/index")

getGameViewR :: GameId -> Handler RepHtml
getGameViewR gameId = do
    game <- runDB $ get404 gameId
    let stats = gameGameStats game
    champions <- championsByName
    defaultLayout $ do
        setTitle "Game"
        addScript $ StaticR js_bootstrap_bootstrap_twipsy_js
        addScript $ StaticR js_bootstrap_bootstrap_popover_js
        addScript $ StaticR js_bootstrap_bootstrap_tabs_js
        addScript $ StaticR js_jqplot_jquery_jqplot_min_js
        addScript $ StaticR js_jqplot_jqplot_barRenderer_min_js
        $(widgetFile "game/view")
    where
        playerDetails player champions = $(widgetFile "game/player-details")
        statNames = concatMap snd gridStats
        statData game = 
            let players = gsBlueTeam (gameGameStats game) ++ gsPurpleTeam (gameGameStats game)
                playerData p = map (\s -> fromMaybe 0 $ lookupStat s p) statNames
             in catMaybes $ map (\p -> playerData <$> gameLookupPlayer p game) players


postGameCreateR :: Handler RepJson
postGameCreateR = do
    bss <- lift consume
    let body = foldr (BS.append) BS.empty bss
    case (parseOnly (rawGames <$> json) body :: Either String (Result [GameStats])) of
        Right (Success games)   -> insertGames games
        Right (Error msg)       -> return . RepJson . toContent . encode . String . pack $ msg
        Left msg                -> return . RepJson . toContent . encode . String . pack $ msg
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
