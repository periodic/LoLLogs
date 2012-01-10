module Model.Spell where

import Import

import qualified Data.Map as M

type SpellMap = M.Map Int Spell

spellsById :: Handler SpellMap
spellsById = do
    spells <- (runDB $ selectList [] [Asc SpellName]) :: Handler [(SpellId, Spell)]
    return . M.fromList . map (\(_,spell) -> (spellIdent spell, spell)) $ spells

lookupSpell :: Int -> SpellMap -> Maybe Spell
lookupSpell = M.lookup

spellsAsList :: SpellMap -> [(Int, Spell)]
spellsAsList = M.toList

spellImage :: Spell -> Widget
spellImage spell = do
    addStylesheet $ StaticR css_spell_thumbnail_sprite_css
    [whamlet|<div class="spell-thumbnail spell-thumbnail-#{spellImageName spell} spell-id-#{spellIdent spell}" title="#{spellName spell}">|]

spellImageLink :: LoLLogsWebAppRoute -> Spell -> Widget
spellImageLink route spell = do
    addStylesheet $ StaticR css_spell_thumbnail_sprite_css
    [whamlet|<a href=@{route} class="spell-thumbnail spell-thumbnail-#{spellImageName spell}" title="#{spellName spell}">|]
