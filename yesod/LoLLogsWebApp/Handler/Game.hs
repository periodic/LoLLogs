module Handler.Game where

import Import

import Data.Enumerator.List (consume)
import qualified Data.ByteString as BS
import Data.Aeson (fromJSON, json, Result(..), encode, Value(..))
import Data.Attoparsec (parseOnly)
import Data.Text (pack)
import Data.Time (getCurrentTime)

import Model.Champion

getGameIndexR :: Handler RepHtml
getGameIndexR = do
    games <- runDB $ selectList [] []
    defaultLayout $ do
        setTitle "Game Index"
        $(widgetFile "game-index")

getGameRankedR :: Handler RepHtml
getGameRankedR = do
    games <- runDB $ selectList [GameRanked ==. True] []
    defaultLayout $ do
        setTitle "Game Index"
        $(widgetFile "game-index")

getGameViewR :: GameId -> Handler RepHtml
getGameViewR gameId = do
    game <- runDB $ get404 gameId
    champions <- championsByName
    defaultLayout $ do
        setTitle "Game"
        $(widgetFile "game-view")


postGameCreateR :: Handler RepJson
postGameCreateR = do
    bss <- lift consume
    let body = foldr1 (BS.append) bss
    case (parseOnly (fromJSON <$> json) body :: Either String (Result [GameStats])) of
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
