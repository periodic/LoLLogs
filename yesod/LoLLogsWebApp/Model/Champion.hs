module Model.Champion where

import Import

import qualified Data.Map as M
import qualified Data.Text as T

type ChampionMap = M.Map Text Champion

championsByName :: Handler ChampionMap
championsByName = do
    champs <- (runDB $ selectList [] [Asc ChampionName]) :: Handler [(ChampionId, Champion)]
    return . M.fromList . map (\(_,champ) -> (championSkinName champ, champ)) $ champs

lookupChamp :: Text -> ChampionMap -> Maybe Champion
lookupChamp = M.lookup

champsAsList :: ChampionMap -> [(Text, Champion)]
champsAsList = M.toList

championImageUrl :: Champion -> StaticRoute
championImageUrl champ = StaticRoute ["img", "champions", T.concat [championSkinName champ, "_Square_0.png"]] []

championImage :: Champion -> Widget
championImage champ = do
    addStylesheet $ StaticR css_champion_thumbnail_sprite_css
    [whamlet|<div class="champion-thumbnail thumbnail-#{championSkinName champ}">|]
championImageLink :: LoLLogsWebAppRoute -> Champion -> Widget
championImageLink route champ = do
    addStylesheet $ StaticR css_champion_thumbnail_sprite_css
    [whamlet|<a href=@{route} class="champion-thumbnail thumbnail-#{championSkinName champ}">|]
