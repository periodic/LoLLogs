module Model.Item where

import Import

import qualified Data.Map as M
import qualified Data.Text as T

type ItemMap = M.Map Int Item

itemsById :: Handler ItemMap
itemsById = do
    items <- (runDB $ selectList [] [Asc ItemName]) :: Handler [(ItemId, Item)]
    return . M.fromList . map (\(_,item) -> (itemIdent item, item)) $ items

lookupItem :: Int -> ItemMap -> Maybe Item
lookupItem = M.lookup

itemsAsList :: ItemMap -> [(Int, Item)]
itemsAsList = M.toList

itemImage :: Item -> Widget
itemImage item = do
    addStylesheet $ StaticR sprites_items_css
    [whamlet|<div class="item-thumbnail sprite-items-#{cleanIconPath $ itemIconPath item}" title="#{itemName item}">|]

itemImageLink :: LoLLogsWebAppRoute -> Item -> Widget
itemImageLink route item = do
    addStylesheet $ StaticR sprites_items_css
    [whamlet|<a href=@{route} class="item-thumbnail sprite-items-#{cleanIconPath $ itemIconPath item}" title="#{itemName item}">|]

itemImageEmpty :: Widget
itemImageEmpty = do
    addStylesheet $ StaticR sprites_items_css
    [whamlet|<div class="item-thumbnail sprite-items-EmptyIcon" title="Empty">|]

cleanIconPath :: Text -> Text
cleanIconPath = T.takeWhile (/= '.')
