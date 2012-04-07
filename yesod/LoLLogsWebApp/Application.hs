{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withLoLLogsWebApp
    , withDevelAppPort
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, flushLogger)
import Network.Wai.Middleware.RequestLogger
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Store

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Game
import Handler.Champion
--import Handler.ChartExample
import Handler.Summoner

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "LoLLogsWebApp" resourcesLoLLogsWebApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withLoLLogsWebApp :: AppConfig DefaultEnv () -> Logger -> (Application -> IO ()) -> IO ()
withLoLLogsWebApp conf logger f = do
    s <- staticSite
    dbconf <- withYamlEnvironment "config/mongoDB.yml" (appEnv conf)
            $ either error return . Database.Persist.Store.loadConfig
    Database.Persist.Store.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        let h = LoLLogsWebApp conf logger s p
        defaultRunner (f . logWare) h
    where
#ifdef DEVELOPMENT
        logWare = logHandleDev (\msg -> logBS logger msg >> flushLogger logger)
#else
        logWare = logStdout
#endif

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withLoLLogsWebApp
