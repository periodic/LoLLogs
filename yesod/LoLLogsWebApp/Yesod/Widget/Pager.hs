module Yesod.Widget.Pager ( paginateSelectList
                          , pager
                          , PagerOptions(..)
                          ) where


import Prelude hiding (writeFile, readFile)
import Yesod
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

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
    , urlGenerator :: Int -> Html
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
    , urlGenerator = defaultUrlGenerator
    }

defaultUrlGenerator :: Int -> Html
defaultUrlGenerator i = toHtml $ "?p=" ++ show i

-- paginateSelectList :: (PersistEntity val, PersistBackend b m) => Int -> [Filter val] -> [SelectOpt val] -> Handler ([(Key b val, val)], PagerOptions)
paginateSelectList pSize filters opts = do
    page <- fromMaybe 1 <$> (runInputGet $ iopt intField "p")
    let offset = max ((page - 1) * pSize) 0
    (results, totalCount) <- runDB $ do -- Single transaction by using one runDB call.
        results    <- selectList filters (LimitTo pSize : OffsetBy offset : opts)
        totalCount <- count filters
        return (results, totalCount)
    let pagerOpts = defaultPagerOptions { pageSize = pSize, currentPage = page, maxPage = totalCount `div` pSize + 1}
    return (results, pagerOpts)

pager :: PagerOptions -> GWidget sub master ()
pager opts = 
    let p = currentPage opts
        m = maxPage opts
        minP = maybe 1 (\context -> max 1 (currentPage opts - context)) $ pageContext opts
        maxP = maybe m (\context -> min m (currentPage opts + context)) $ pageContext opts
        prevP = max 0 (p - 1)
        nextP = min m (p + 1)
        pageNums = [minP .. maxP]

        isMultiplePages = m > 1
        isFirstPage = p <= 1
        isLastPage = p >= m
        hasPrevEllip = minP > 1
        hasNextEllip = maxP < m
    in [whamlet|
$if isMultiplePages
    <div class=#{cssClass opts}>
        <ul>
            $if showFirst opts
                <li.first.prev :isFirstPage:.disabled>
                    <a href=#{urlGenerator opts 0}>
                        First
            $if showPrev opts
                <li.prev :isFirstPage:.disabled>
                    <a href=#{urlGenerator opts prevP}>
                        Prev
            $if hasPrevEllip
                <li.disabled>
                    <a href=#>
                        &hellip;
            $forall n <- pageNums
                <li :(==) n p:.active>
                    <a href=#{urlGenerator opts n}>
                        #{n}
            $if hasNextEllip
                <li.disabled>
                    <a href=#>
                        &hellip;
            $if showNext opts
                <li.next :isLastPage:.disabled>
                    <a href=#{urlGenerator opts nextP}>
                        Next
            $if showLast opts
                <li.last.next :isLastPage:.disabled>
                    <a href=#{urlGenerator opts m}>
                        Last
|]
