{-# LANGUAGE OverloadedStrings #-}
module Database.MongoDB.GameLogs where

import Database.MongoDB as Mongo
import Data.GameLog

import Data.Int


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
