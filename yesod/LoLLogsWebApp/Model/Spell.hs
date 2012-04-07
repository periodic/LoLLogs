module Model.Spell where

import Import

import qualified Data.Map as M

type SpellMap = M.Map Int Spell

spellsById :: Handler SpellMap
spellsById = do
    spells <- (runDB $ selectList [] [Asc SpellName]) :: Handler [Entity Spell]
    return . M.fromList . map (\spell -> (spellIdent spell, spell)) . map entityVal $ spells

lookupSpell :: Int -> SpellMap -> Maybe Spell
lookupSpell = M.lookup

spellsAsList :: SpellMap -> [(Int, Spell)]
spellsAsList = M.toList

spellImage :: Spell -> Widget
spellImage spell = do
    addStylesheet $ StaticR sprites_spells_css
    [whamlet|<div class="spell-thumbnail sprite-spells-#{spellImageName spell} spell-id-#{spellIdent spell}" title="#{spellName spell}">|]

spellImageLink :: Route LoLLogsWebApp -> Spell -> Widget
spellImageLink route spell = do
    addStylesheet $ StaticR sprites_spells_css
    [whamlet|<a href=@{route} class="spell-thumbnail sprite-spells-#{spellImageName spell}" title="#{spellName spell}">|]
