module Widget.GameList where

import Import

import Model.Champion

import Yesod.Widget.Pager

gameList :: (Text -> Bool) -- ^ Summoner(s) to highlight.
         -> ChampionMap
         -> [(GameId, Game)]
         -> PagerOptions
         -> Widget
gameList isMe champions games pagerOpts = do
    addScript $ StaticR js_bootstrap_bootstrap_twipsy_js
    $(widgetFile "game/list")
    where
        portraits :: Game -> Widget
        portraits game = $(widgetFile "game/champions")


