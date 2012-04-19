module Yesod.Widget.Pager ( paginateSelectList
                          , pager
                          , PagerOptions(..)
                          ) where


import Prelude hiding (writeFile, readFile)
import Foundation
import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

import Blaze.ByteString.Builder
import Text.Blaze

data PagerOptions = PagerOptions
    { showNext :: Bool
    , showPrev :: Bool
    , showFirst :: Bool
    , showLast :: Bool
    , showPages :: Bool
    , pageContext :: Maybe Int
    , cssClass :: Text
    , currentPage :: Int
    , maxPage :: Int
    , pageSize :: Int
    }

defaultPagerOptions :: PagerOptions
defaultPagerOptions = PagerOptions
    { showNext  = True
    , showPrev  = True
    , showFirst = True
    , showLast  = True
    , showPages = True
    , pageContext = Just 10
    , cssClass = "pagination"
    , currentPage = 0
    , maxPage = 0
    , pageSize = 10
    }

defaultUrlGenerator :: [(Text, Text)] -> Int -> Html
defaultUrlGenerator params i = toHtml . T.cons '?' . T.intercalate "&" $ paramStrings
    where
        newParams = ("p", T.pack . show $ i) : filter (not . (== "p") . fst) params
        paramStrings = map (\(k,v) -> T.concat [k, "=", v]) newParams

-- paginateSelectList :: (PersistEntity val, PersistBackend b m) => Int -> [Filter val] -> [SelectOpt val] -> Handler ([(Key b val, val)], PagerOptions)
paginateSelectList pSize filters opts = do
    page <- fromMaybe 1 <$> (runInputGet $ iopt intField "p")
    let offset = max ((page - 1) * pSize) 0
    (results, totalCount) <- runDB $ do -- Single transaction by using one runDB call.
        results    <- selectList filters (LimitTo pSize : OffsetBy offset : opts)
        totalCount <- count filters
        return (results, totalCount)
    let pagerOpts = defaultPagerOptions { pageSize = pSize, currentPage = page, maxPage = (totalCount - 1) `div` pSize + 1}
    return (results, pagerOpts)

pager :: PagerOptions -> GWidget sub master ()
pager opts = do
    curParams <- lift $ fmap reqGetParams getRequest
    let p = currentPage opts
        m = maxPage opts

        {- TODO: this can probably be cleaned up.
         -
         - * Basically, we set the minimum to either 1 or the current page
         - minus our context, whichever is in bounds.
         -
         - * Then we set the max to either the max page or the min page plus
         - twice the context, whichever is in bounds.  This makes the max
         - stretch further if little of the context was used on the lower
         - bound.
         -
         - * But now what if the max didn't use up the whole context?  We have
         - to lower the min context to possibly the max minus twice the context
         - if that's also in bounds.
         -}
        minP' = maybe 1 (\context -> max 1 (currentPage opts - context)) $ pageContext opts
        maxP  = maybe m (\context -> min m (minP' + 2 * context)) $ pageContext opts
        minP  = max 1 $ maybe 1 (\context -> min minP' (maxP - 2 * context)) $ pageContext opts

        prevP = max 0 (p - 1)
        nextP = min m (p + 1)
        pageNums = [minP .. maxP]

        isMultiplePages = m > 1
        isFirstPage = p <= 1
        isLastPage = p >= m
        hasPrevEllip = minP > 1
        hasNextEllip = maxP < m

        mkUrl = defaultUrlGenerator curParams
    [whamlet|
$if isMultiplePages
    <div class=#{cssClass opts}>
        <ul>
            $if showFirst opts
                <li.first.prev :isFirstPage:.disabled>
                    <a href=#{mkUrl 0}>
                        First
            $if showPrev opts
                <li.prev :isFirstPage:.disabled>
                    <a href=#{mkUrl prevP}>
                        Prev
            $if hasPrevEllip
                <li.disabled>
                    <a href=#>
                        &hellip;
            $forall n <- pageNums
                <li :(==) n p:.active>
                    <a href=#{mkUrl n}>
                        #{n}
            $if hasNextEllip
                <li.disabled>
                    <a href=#>
                        &hellip;
            $if showNext opts
                <li.next :isLastPage:.disabled>
                    <a href=#{mkUrl nextP}>
                        Next
            $if showLast opts
                <li.last.next :isLastPage:.disabled>
                    <a href=#{mkUrl m}>
                        Last
|]
