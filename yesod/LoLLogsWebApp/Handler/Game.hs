module Handler.Game where

import Import
import Settings.StaticFiles

import Data.Enumerator.List (consume)
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Data.Aeson (json, Result(..), encode, Value(..))
import Data.Attoparsec (parseOnly)
import Data.Text (pack)
import Data.Time (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)

import Model.Champion

champPortrait :: Text -> ChampionMap -> Widget
champPortrait skinName champions = $(widgetFile "game/champion-portrait")

portraits :: ChampionMap -> Game -> Widget
portraits champions game = $(widgetFile "game/champions")

getGameIndexR :: Handler RepHtml
getGameIndexR = do
    games <- runDB $ selectList [] []
    champions <- championsByName
    defaultLayout $ do
        setTitle "Game Index"
        $(widgetFile "game/index")

getGameRankedR :: Handler RepHtml
getGameRankedR = do
    games <- runDB $ selectList [GameRanked ==. True] []
    champions <- championsByName
    defaultLayout $ do
        setTitle "Game Index"
        $(widgetFile "game/index")

getGameViewR :: GameId -> Handler RepHtml
getGameViewR gameId = do
    game <- runDB $ get404 gameId
    champions <- championsByName
    defaultLayout $ do
        setTitle "Game"
        $(widgetFile "game/view")


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
            let gameId = gsgameId . gameGameStats $ game
            mGame <- runDB . getBy . UniqueGameId $ fromIntegral gameId
            case mGame of 
                Just _  -> return ()
                Nothing -> (runDB $ insert game) >> return ()
