{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withLoLLogsWebApp
    , withDevelAppPort
    ) where

{-
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
import Network.HTTP.Conduit (newManager, def)
-}

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import qualified Database.Persist.Store
import Network.HTTP.Conduit (newManager, def)

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
{-
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
-}

-- for yesod devel
{-
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withLoLLogsWebApp
-}



-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO LoLLogsWebApp
makeFoundation conf setLogger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/mongoDB.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    return $ LoLLogsWebApp conf setLogger s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
