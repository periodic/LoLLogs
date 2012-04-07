module Model.Item where

import Import

import qualified Data.Map as M
import qualified Data.Text as T

type ItemMap = M.Map Int Item

itemsById :: Handler ItemMap
itemsById = do
    items <- (runDB $ selectList [] [Asc ItemName]) :: Handler [Entity Item]
    return . M.fromList . map (\item -> (itemIdent $ entityVal item, entityVal item)) $ items

lookupItem :: Int -> ItemMap -> Maybe Item
lookupItem = M.lookup

itemsAsList :: ItemMap -> [(Int, Item)]
itemsAsList = M.toList

itemImage :: Item -> Widget
itemImage item = do
    addStylesheet $ StaticR css_item_thumbnail_sprite_css
    [whamlet|<div class="item-thumbnail item-thumbnail-#{cleanIconPath $ itemIconPath item}" title="#{itemName item}">|]

itemImageLink :: Route LoLLogsWebApp -> Item -> Widget
itemImageLink route item = do
    addStylesheet $ StaticR css_item_thumbnail_sprite_css
    [whamlet|<a href=@{route} class="item-thumbnail item-thumbnail-#{cleanIconPath $ itemIconPath item}" title="#{itemName item}">|]

itemImageEmpty :: Widget
itemImageEmpty = do
    addStylesheet $ StaticR css_item_thumbnail_sprite_css
    [whamlet|<div class="item-thumbnail item-thumbnail-EmptyIcon" title="Empty">|]

cleanIconPath :: Text -> Text
cleanIconPath = T.takeWhile (/= '.')
