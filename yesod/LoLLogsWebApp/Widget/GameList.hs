module Widget.GameList where

import Import

import Model.Champion

import Yesod.Widget.Pager

gameList :: Maybe Text -- ^ Summoner(s) to highlight.
         -> ChampionMap
         -> [(GameId, Game)]
         -> PagerOptions
         -> Widget
gameList mSummonerName champions games pagerOpts = do
    addScript $ StaticR js_bootstrap_bootstrap_twipsy_js
    $(widgetFile "game/list")
    where
        isMe = case mSummonerName of
                Just name -> (name ==)
                Nothing   -> const False
        portraits :: [Text] -> Game -> Widget
        portraits players game = $(widgetFile "game/champions")
        playerWon game = case mSummonerName >>= flip gameLookupPlayer game of
                        Just player -> psEloChange player > 0
                        Nothing     -> False
