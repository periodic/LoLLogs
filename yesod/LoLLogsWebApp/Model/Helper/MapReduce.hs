{-# LANGUAGE OverloadedStrings, FunctionalDependencies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Database.MongoDB.MapReduceHelper ( execute
                                        , GroupOp(..)
                                        , Queryable(..)
                                        , MRQuery
                                        , buildQuery
                                        , addColumn
                                        , showDocument
                                        , (.==)
                                        , (.<)
                                        , (.>)
                                        , (.<=)
                                        , (.>=)
                                        , exists
                                        ) where

import Database.MongoDB as Mongo

import Prelude
import Data.Int
import Data.List (union, intercalate)
import Data.UString as S (UString, concat)
import qualified Data.Map as M

import Control.Monad.State as St


instance Queryable model where
    data QueryColumn :: model -> typ
    querySelector       :: QueryColumn model typ -> Javascript
    queryReduceOp       :: QueryColumn model typ -> Javascript
    queryColumnName     :: QueryColumn model typ -> Javascript
    queryFilter         :: QueryColumn model typ -> Value -> Document
    queryCastResult     :: QueryColumn model typ -> Value -> typ

{- | Execute a map-reduce query, returning a either a list of results as touples from values to maps of data, or an error.
 -}
execute :: MapReduce -> IO (Either Failure [(Label, M.Map Label Value)])
execute query = do
    conn <- runIOE . connect $ host "localhost"
    result <- access conn UnconfirmedWrites "LoLLogsWebApp" . runMR' $ query
    case result of
        Left err -> return $ Left err
        Right doc -> return $ do
            results <- Mongo.lookup "results" doc
            let makeRecord doc = do
                id <- Mongo.lookup "_id" doc
                values <- Mongo.lookup "value" doc
                return (id, M.fromList $ map (\(l := v) -> (l,v)) values) :: Either Failure (Label, M.Map Label Value)
            mapM makeRecord results

{- | Reduce operators.
 -}
data GroupOp = GroupAvg
             | GroupTotal
             | GroupFirst
             deriving (Show, Eq)

simple group

{- | Return an group opperations aggregation funciton.
 -}
groupAggregator :: GroupOp -> UString
groupAggregator (GroupAvg) = "+";
groupAggregator (GroupTotal) = "+";
groupAggregator (GroupFirst) = "||";

{- | Get a group operator's finalizer expression.
 -}
groupFinalizer :: GroupOp -> UString -> UString
groupFinalizer (GroupAvg)   field = S.concat ["result.", field, " = val.", field, " / val._count;"]
groupFinalizer (GroupTotal) field = S.concat ["result.", field, " = val.", field, ";"]
groupFinalizer (GroupFirst) field = S.concat ["result.", field, " = val.", field, ";"]

{- | The type class of a queryable column.
 -}
class (Val typ) => QueryColumn column typ | column -> typ where
    querySelector :: column -> Either UString Javascript
    queryReduceOp :: column -> GroupOp
    queryColumnName :: column -> Either UString Javascript
    queryFilterSelector :: column -> Either UString Javascript
    queryFilterSelector = error "No selector defined for filters on this column."
    queryFilter :: column -> Value -> Document
    queryFilter col v = [queryFilterSelector col := v ]

data QueryFilter = forall column typ. (QueryColumn column typ) => QueryFilter FilterOp column typ
                 | QueryAll [QueryFilter]
                 | QueryAny [QueryFilter]

data FilterOp = FilterEQ
              | FilterLT
              | FilterGT
              | FilterLE
              | FilterGE
              | FilterExists

parseFilters :: [QueryFilter] -> Document
parseFilters []     = []
parseFilters fs | (f:[]) <- fs = convertFilter f
                | otherwise    = convertFilter . QueryAll $ fs
    where
        convertFilter f | (QueryAll fs) <- f = ["$and" =: map convertFilter fs]
                        | (QueryAny fs) <- f = ["$or"  =: map convertFilter fs]
                        | (QueryFilter FilterEQ col v) <- f = queryFilter col $ val v
                        | (QueryFilter FilterLT col v) <- f = queryFilter col $ Doc ["$lt" =: v]
                        | (QueryFilter FilterGT col v) <- f = queryFilter col $ Doc ["$gt" =: v]
                        | (QueryFilter FilterLE col v) <- f = queryFilter col $ Doc ["$le" =: v]
                        | (QueryFilter FilterGE col v) <- f = queryFilter col $ Doc ["$ge" =: v]
                        | (QueryFilter FilterExists col v) <- f = queryFilter col $ Doc ["$exists" =: True]

(.==) :: (Eq typ, Val typ, QueryColumn column typ) => column -> typ -> QueryFilter
(.==) = QueryFilter FilterEQ
(.<), (.>), (.<=), (.>=) :: (Ord typ, Val typ, QueryColumn column typ) => column -> typ -> QueryFilter
(.<)  = QueryFilter FilterLT
(.>)  = QueryFilter FilterGT
(.<=) = QueryFilter FilterLE
(.>=) = QueryFilter FilterGE
exists col = QueryFilter FilterExists col undefined

newtype MRQuery a = Q {
    runQuery :: State QueryState a
} deriving(Monad, Functor, MonadState QueryState)

data QueryState = QueryState
    { keyField :: UString
    , fields   :: M.Map UString (UString, GroupOp)
    } deriving (Show)

{- | Build a MapReduce query. -}
buildQuery :: QueryColumn column typ => Collection -> column -> [QueryFilter] -> MRQuery () -> MapReduce
buildQuery collection keyCol filters query = 
    let initState = QueryState (querySelector keyCol) M.empty
        (QueryState key fieldMap) = execState (runQuery query) initState
        fields = M.toList fieldMap
        mapFields = S.concat . map (\(field,(selector, _)) -> S.concat ["result.", field, " = ", selector, ";"]) $ fields
        mapFunc = Javascript [] $ S.concat ["function () { var key = ", key, "; var result = {_count: 1};", mapFields, "emit(key, result); }"]

        reduceFields = S.concat . map (\(field,(_, op)) -> S.concat ["result.", field, " = result.", field, groupAggregator op, "v.", field, ";"]) $ fields
        reduceFunc = Javascript [] $ S.concat ["function (key, vals) { var result = vals[0]; for (var i = 1; i < vals.length; i++) { var v = vals[i]; result._count = result._count + v._count; ", reduceFields, "}; return result; }"]

        finalizeFields = S.concat . map (\(field, (_, op)) -> groupFinalizer op field) $ fields
        finalizeFunc = Javascript [] $ S.concat ["function (key, val) { var result = {_count: val._count}; ", finalizeFields, "return result; }"]
     in MapReduce collection mapFunc reduceFunc (parseFilters filters) [] 0 Inline (Just finalizeFunc) [] False

addColumn :: QueryColumn column typ => column -> MRQuery ()
addColumn column = do
    let col    = queryColumnName column
        select = querySelector column
        op     = queryReduceOp column
    addField col (select, op)
    where
        addField col dat = St.modify $ \s@(QueryState _ fields) -> s { fields = M.insert col dat fields }

setKey :: QueryColumn column typ => column -> MRQuery ()
setKey column = do
    let select = querySelector column
    St.modify $ (\s -> s { keyField = select })

showDocument :: Document -> String
showDocument ls = "{" ++ (intercalate ", " $ map (\(l := v) -> show l ++ ": " ++ showDocumentR v) ls) ++ "}"
    where
        showDocumentR :: Value -> String
        showDocumentR (Doc ls) = showDocument ls
        showDocumentR (Array as) = "[" ++ (intercalate ", " $ map show as) ++ "]"
        showDocumentR other = show other
