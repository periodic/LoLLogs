module Model.Champion where

import Import

import qualified Data.Map as M

type ChampionMap = M.Map Text Champion

championsByName :: Handler ChampionMap
championsByName = do
    champs <- (runDB $ selectList [] []) :: Handler [(ChampionId, Champion)]
    return . M.fromList . map (\(_,champ) -> (championSkinName champ, champ)) $ champs

lookupChamp :: Text -> ChampionMap -> Maybe Champion
lookupChamp = M.lookup

