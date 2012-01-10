{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, PatternGuards, FlexibleContexts #-}
module Model.Helper.MapReduce ( execute
                              , runMapReduce
                              , Model.Helper.MapReduce.mapReduce
                              , Queryable(..)
                              , MRData
                              , MRBackend
                              , simpleKey
                              , simpleMap
                              , simpleReduce
                              , simpleFinalize
                              , simpleFinalizeAvg
                              , simpleFilter
                              , simpleMapFunc
                              , simpleReduceFunc
                              , simpleFinalizeFunc
                              , GroupOp(..)
                              , buildQuery
                              , QuerySelect(..)
                              , (.==)
                              , (.<)
                              , (.>)
                              , (.<=)
                              , (.>=)
                              , (.<-)
                              , exists
                              , anyFilter
                              , allFilter
                              , t2u
                              , u2t
                              , unJS
                              , wrapJS
                              , getResultValue
                              , getResultCount
                              -- From MongoDB
                              , MapReduce
                              , Label
                              , Value
                              ) where

import Database.MongoDB as Mongo hiding (selector)

import Prelude
import Database.Persist.Base
import qualified Data.UString as S (pack, unpack, concat)
import Data.Text as T (Text, unpack, pack)
import Data.Maybe (catMaybes)
import Data.String
import Control.Applicative (Applicative(..))
import qualified Data.Map as M

t2u :: Text -> UString
t2u = S.pack . T.unpack

u2t :: UString -> Text
u2t = T.pack . S.unpack

catJS :: [Javascript] -> Javascript
catJS = wrapJS . S.concat . map unJS

bracketJS :: Javascript -> Javascript
bracketJS js = catJS ["{", js, "}"]

unJS :: Javascript -> UString
unJS (Javascript _ code) = code

wrapJS :: UString -> Javascript
wrapJS = Javascript []

type MRData = M.Map Label Value
type MRBackend = Action

instance Val Text where
    val = String . S.pack . T.unpack
    cast' v = case v of
        String str -> Just . T.pack . S.unpack $ str
        _          -> Nothing

instance IsString Javascript where
    fromString = Javascript [] . S.pack

class PersistEntity model => Queryable model where
    -- Column definitions
    data QueryColumn model :: * -> *
    queryColumnName     :: QueryColumn model typ -> UString    -- ^ The column name
    queryFilter         :: QueryColumn model typ -> Value -> Document  -- ^ Produce the document to be used as a filter when given a value.

    -- Define the map function
    queryKeyCode :: QueryColumn model typ -> Javascript -- ^ Used to select when used as a key.
    queryMapCode :: QueryColumn model typ -> Javascript -- ^ Should set fields on "result" from "this".  Run once for each document.

    -- Define the reduce funciton
    queryReduceCode     :: QueryColumn model typ -> Javascript -- ^ Used to merge fields from the map set.  Should merge "v" into "result", touching only its fields.
    queryReduceCode = simpleReduce GroupTotal
    -- Define the finalize function
    queryFinalizeCode   :: QueryColumn model typ -> Javascript -- ^ Used to finalize the result.  Should work on "result" and "v". 
    queryFinalizeCode = simpleFinalize
    -- Utilitiy functions
    queryCastResult     :: (Val typ) => QueryColumn model typ -> Value -> Maybe typ -- ^ Cast a result of type Value into the type for this column.
    queryCastResult _ v = cast' v


    -- Model-wide definitions.
    queryMapFunc :: QueryColumn model typ -> forall typ0. [QueryColumn model typ0] -> Javascript
    queryMapFunc = simpleMapFunc

    queryReduceFunc :: forall typ0. [QueryColumn model typ0] -> Javascript
    queryReduceFunc = simpleReduceFunc

    queryFinalizeFunc :: forall typ0. [QueryColumn model typ0] -> Javascript
    queryFinalizeFunc = simpleFinalizeFunc

    queryCollection :: QueryColumn model typ -> UString
    queryCollection _ = S.pack . entityName $ entityDef (undefined :: model)

{- | Reduce operators.
 -}
data GroupOp = GroupAvg
             | GroupTotal
             | GroupFirst
             deriving (Show, Eq)
{- | Return an group opperations aggregation funciton.
 -}
groupAggregator :: GroupOp -> UString
groupAggregator (GroupAvg) = "+";
groupAggregator (GroupTotal) = "+";
groupAggregator (GroupFirst) = "||";

simpleKey :: Queryable model => UString -> QueryColumn model typ -> Javascript
simpleKey selector _ = wrapJS $ S.concat ["this.", selector]

simpleMap :: Queryable model => UString -> QueryColumn model typ -> Javascript
simpleMap selector col =
    let field = queryColumnName col
     in Javascript [] $ S.concat ["result.", field, " = this.", selector, ";"]

{- | Create a simple grouping function based on an operator and a selector.
 -}
simpleReduce :: Queryable model => GroupOp -> QueryColumn model typ -> Javascript
simpleReduce op col =
    let field = queryColumnName col
     in Javascript [] $ S.concat ["result.", field, " = result.", field, groupAggregator op, "v.", field, ";"]

simpleFinalize :: Queryable model => QueryColumn model typ -> Javascript
simpleFinalize col =
    let field = queryColumnName col
     in Javascript [] $ S.concat ["result.", field, " = v.", field, ";"]

simpleFinalizeAvg :: Queryable model => QueryColumn model typ -> Javascript
simpleFinalizeAvg col =
    let field = queryColumnName col
     in Javascript [] $ S.concat ["result.", field, " = v.", field, " / v._count;"]

simpleFilter :: Queryable model => UString -> QueryColumn model typ -> Value -> Document
simpleFilter selector _ v = [ selector := v ]

data QueryFilter model = forall typ. Val typ => QueryFilter FilterOp (QueryColumn model typ) typ
                       | forall typ. Val typ => QueryFilterMulti FilterOp (QueryColumn model typ) [typ]
                       | QueryAll [QueryFilter model]
                       | QueryAny [QueryFilter model]

data QuerySelect model = forall typ. QuerySelect (QueryColumn model typ)

data FilterOp = FilterEQ
              | FilterLT
              | FilterGT
              | FilterLE
              | FilterGE
              | FilterIn
              | FilterExists

parseFilters :: Queryable model => [QueryFilter model] -> Document
parseFilters []     = []
parseFilters filters | (f:[]) <- filters = convertFilter f
                     | otherwise         = convertFilter . QueryAll $ filters
    where
        convertFilter f | (QueryAll fs) <- f = concat $ map convertFilter fs
                        | (QueryAny fs) <- f = if length fs == 1
                                               then concat $ map convertFilter fs
                                               else ["$or"  =: map convertFilter fs]
                        | (QueryFilter FilterEQ col v) <- f = queryFilter col $ val v
                        | (QueryFilter FilterLT col v) <- f = queryFilter col $ Doc ["$lt" =: v]
                        | (QueryFilter FilterGT col v) <- f = queryFilter col $ Doc ["$gt" =: v]
                        | (QueryFilter FilterLE col v) <- f = queryFilter col $ Doc ["$le" =: v]
                        | (QueryFilter FilterGE col v) <- f = queryFilter col $ Doc ["$ge" =: v]
                        | (QueryFilter FilterExists col _) <- f = queryFilter col $ Doc ["$exists" =: True]

                        | (QueryFilterMulti FilterIn col vs) <- f = queryFilter col $ Doc ["$in" =: vs]

                        | _ <- f = error "Invalid filter."

(.==) :: (Eq typ, Val typ, Queryable model) => QueryColumn model typ -> typ -> QueryFilter model
(.==) = QueryFilter FilterEQ

(.<), (.>), (.<=), (.>=) :: (Ord typ, Val typ, Queryable model) => QueryColumn model typ -> typ -> QueryFilter model
(.<)  = QueryFilter FilterLT
(.>)  = QueryFilter FilterGT
(.<=) = QueryFilter FilterLE
(.>=) = QueryFilter FilterGE

(.<-) :: (Ord typ, Val typ, Queryable model) => QueryColumn model typ -> [typ] -> QueryFilter model
(.<-) = QueryFilterMulti FilterIn

exists :: (Val typ, Queryable model) => QueryColumn model typ -> QueryFilter model
exists col = QueryFilter FilterExists col undefined

allFilter :: Queryable model => [QueryFilter model] -> QueryFilter model
allFilter = QueryAll

anyFilter :: Queryable model => [QueryFilter model] -> QueryFilter model
anyFilter = QueryAny

{-
newtype MRQuery a = Q {
    runQuery :: State QueryState a
} deriving(Monad, Functor, MonadState QueryState)

data QueryState = QueryState
    { keyField :: UString
    , fields   :: M.Map UString (Javascript, Javascript, Javascript) -- ^ The map from field names to map, reduce, and filter javascript for them.
    } deriving (Show)
-}

{- | Build a MapReduce query. -}
buildQuery :: Queryable model => QueryColumn model typ  -- ^ The column to use a key.
           -> [QueryFilter model]                       -- ^ A list of filters.
           -> forall typ0. [QueryColumn model typ0]     -- ^ A list of columns to select for the output.
           -> MapReduce
buildQuery keyCol filters fields =
    let collection = queryCollection keyCol

        mapFunc      = queryMapFunc keyCol fields
        reduceFunc   = queryReduceFunc fields
        finalizeFunc = queryFinalizeFunc fields

     in MapReduce collection mapFunc reduceFunc (parseFilters filters) [] 0 Inline (Just finalizeFunc) [] False

simpleMapFunc :: Queryable model => QueryColumn model typ -> forall typ0. [QueryColumn model typ0] -> Javascript
simpleMapFunc keyCol fields =
    let key = unJS $ queryKeyCode keyCol
        mapCode      = unJS . catJS . map (bracketJS . queryMapCode) $ fields
     in Javascript [] $ S.concat ["function () { var key = ", key, "; var result = {_count: 1};", mapCode, "emit(key, result); emit('_total', result); }"]

simpleReduceFunc :: Queryable model => forall typ. [QueryColumn model typ] -> Javascript
simpleReduceFunc fields =
    let reduceCode   = unJS . catJS . map (bracketJS . queryReduceCode) $ fields
     in Javascript [] $ S.concat ["function (key, vals) { var result = vals[0]; for (var i = 1; i < vals.length; i++) { var v = vals[i]; result._count = result._count + v._count; ", reduceCode, "}; return result; }"]

simpleFinalizeFunc :: Queryable model => forall typ. [QueryColumn model typ] -> Javascript
simpleFinalizeFunc fields =
    let finalizeCode = unJS . catJS . map (bracketJS . queryFinalizeCode) $ fields
     in Javascript [] $ S.concat ["function (key, v) { var result = {_count: v._count}; ", finalizeCode, "return result; }"]

{- | Execute a map-reduce query, returning a either a list of results as touples from values to maps of data, or an error.
 -}
execute :: MapReduce -> IO (Either Failure [(Label, MRData)])
execute query = do
    conn <- runIOE . connect $ host "localhost"
    result <- access conn UnconfirmedWrites "LoLLogsWebApp" . runMR' $ query
    case result of
        Left err -> return $ Left err
        Right doc -> return $ do
            results <- Mongo.lookup "results" doc
            let makeRecord rec = do
                recid <- Mongo.lookup "_id" rec
                values <- Mongo.lookup "value" rec
                return (recid, M.fromList $ map (\(l := v) -> (l,v)) values)
            return . catMaybes $ map makeRecord results

runMapReduce :: (PersistBackend Action m, Applicative m) => MapReduce -> Action m [(Label, MRData)]
runMapReduce query = do
    result  <- runMR' query
    results <- Mongo.lookup "results" result -- Note, will call fail if no results exist.
    return . catMaybes $ map makeRecord results
    where
        makeRecord :: Document -> Maybe (Label, MRData)
        makeRecord doc = do
            recId  <- Mongo.lookup "_id" doc
            values <- Mongo.lookup "value" doc
            return (recId, M.fromList $ map (\(l := v) -> (l,v)) values)

mapReduce :: (PersistBackend Action m, Applicative m, Queryable model)
           => QueryColumn model typ                     -- ^ The column to use a key.
           -> [QueryFilter model]                       -- ^ A list of filters.
           -> [QueryColumn model typ0]     -- ^ A list of columns to select for the output.
           -> Action m [(Label, MRData)]
mapReduce keyCol filters fields = runMapReduce $ buildQuery keyCol filters fields

-- | Get a result value from the result set, and make sure it is cast to the appropriate type.
getResultValue :: (Val typ, Queryable model) => QueryColumn model typ -> MRData -> Maybe typ
getResultValue col vals = M.lookup (queryColumnName col) vals >>= queryCastResult col

-- | Get the count of documents that went into a result set
getResultCount :: MRData -> Int
getResultCount vals = case (M.lookup "_count" vals >>= cast') of
    Just c  -> c
    Nothing -> 0

