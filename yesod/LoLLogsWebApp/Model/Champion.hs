module Model.Champion where

import Import

import Yesod.Static

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

type ChampionMap = M.Map Text Champion

championsByName :: Handler ChampionMap
championsByName = do
    champs <- (runDB $ selectList [] [Asc ChampionName]) :: Handler [Entity Champion]
    return . M.fromList . map (\champ -> (championSkinName $ entityVal champ, entityVal champ)) $ champs

lookupChamp :: Text -> ChampionMap -> Maybe Champion
lookupChamp = M.lookup

champsAsList :: ChampionMap -> [(Text, Champion)]
champsAsList = M.toList

championImageUrl :: Champion -> StaticRoute
championImageUrl champ = StaticRoute ["img", "champions", T.concat [championSkinName champ, "_Square_0.png"]] []

championImage :: Maybe (Route LoLLogsWebApp) -> Maybe Champion -> Maybe Text -> Widget
championImage mRoute mChamp mText = do
    addStylesheet $ StaticR sprites_champion_thumbnails_css
    let thumbImg = maybe     "Unknown" championSkinName mChamp
    let tooltip  = fromMaybe (maybe "Unknown" championName mChamp) mText
    case mRoute of
        Just route -> [whamlet|<div class="champion-thumbnail sprite-champion-thumbnails-thumbnail-#{thumbImg}" title="#{tooltip}">|]
        Nothing    -> [whamlet|<div class="champion-thumbnail sprite-champion-thumbnails-thumbnail-#{thumbImg}" title="#{tooltip}">|]
