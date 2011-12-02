module Model.Champion where

import Import
import Data.Text (pack)

import qualified Data.Map as M

type ChampionMap = M.Map Text Champion

championsByName :: Handler ChampionMap
championsByName = do
    champs <- (runDB $ selectList [] []) :: Handler [(ChampionId, Champion)]
    return . M.fromList . map (\(_,champ) -> (championName champ, champ)) $ champs

lookupChamp :: String -> ChampionMap -> Maybe Champion
lookupChamp = M.lookup . pack
