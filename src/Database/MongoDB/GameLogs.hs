{-# LANGUAGE OverloadedStrings #-}
module Database.MongoDB.GameLogs where

import Database.MongoDB as Mongo
import Data.GameLog

import Data.Int
import Data.List (union)


mapFunc = Javascript [] "function () { emit(this.gameStats.queueType, { count: 1, length: this.gameStats.gameLength}) }"
reduceFunc = Javascript [] "function (key, values) { var result = {count: 0, length: 0}; values.forEach(function (value) { result.count += value.count; result.length += value.length; }); return result; }"
finalizeFunc = Javascript [] "function (key, value) { return { avg: value.length / value.count, count: value.count}; }"

sort :: Label -> Order
sort field = [field =: (1 :: Int32)]

mr m r f = MapReduce "Game" m r [] [] 0 Inline (Just f) [] False

execute :: IO (Either Failure [(Label, [(Label, Value)])])
execute = do
    conn <- runIOE . connect $ host "localhost"
    result <- access conn UnconfirmedWrites "LoLLogsWebApp" . runMR' $ mr mapFunc reduceFunc finalizeFunc
    case result of
        Left err -> return $ Left err
        Right doc -> return $ do
            results <- Mongo.lookup "results" doc
            let makeRecord doc = do
                id <- Mongo.lookup "_id" doc
                values <- Mongo.lookup "value" doc
                return (id, map (\(l := v) -> (l,v)) values) :: Either Failure (Label, [(Label, Value)])
            mapM makeRecord results


{- | Merge two documents recursively. The default merge will favor the first
 - document if there is a key conflict.  This version will merge the two values
 - of the key if they are either arrays or objects.
 -}
mergeRecursive :: Document -> Document -> Document
mergeRecursive = mergeDocuments -- merge documents initially.
    where
        {- | When two documents merge, we get all the labels, then merge the
         - values for each label in the union of labels. -}
        mergeDocuments d1 d2 =
            let labels = map (\(l := v) -> l)
                ls = labels d1 `union` labels d2
             in map (\l -> l := mergeValue (look l d1) (look l d2)) ls

        {- | When we get only one value, we use that.  We should always get at
         - least one value.  Two documents require special treatment, as do
         - arrays, but for anything else we use the first value.
         -}
        mergeValue (Just a) Nothing  = a
        mergeValue Nothing  (Just a) = a
        mergeValue Nothing  Nothing  = error "Merging two missing values."
        mergeValue (Just a) (Just b) = case (a, b) of
            ((Doc d1)  , (Doc d2)  ) -> Doc $ mergeDocuments d1 d2
            ((Array a1), (Array a2)) -> Array (a1 `union` a2)
            (_         , _         ) -> a


{- | Datatypes to represent types of Fields to search/map/fold against.
 -}
data NumField = IntField [Label] Int
              | FloatField [Label] Float
data StringField = StringField [Label] String

class MongoFilter a where
    toDocument :: a -> Document

instance MongoFilter NumField where
    toDocument (IntField ls i) = nest ls i
    toDocument (FloatField ls f) = nest ls f
instance MongoFilter StringField where
    toDocument (StringField ls s) = nest ls s

{- | Nest some labels and put a value inside.
 -}
nest :: Val a => [Label] -> a -> Document
nest ls v = case nestR ls v of
    Doc d -> d
    _     -> error "Empty label list."
    where
        nestR (l:ls) v = Doc [l := nestR ls v]
        nestR []     v = val v
