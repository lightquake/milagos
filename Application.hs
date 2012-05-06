{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import           Import
import           Model.Loader
import           Settings
import           System.IO
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main
#if DEVELOPMENT
import           Yesod.Logger (Logger, logBS)
import           Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import           Yesod.Logger (Logger, logBS, toProduction)
import           Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import qualified Database.Persist.Store
import           Database.Persist.GenericSql (runMigration)
import           Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
import           Handler.Posts
import           Handler.Rss

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Milagos" resourcesMilagos

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    hSetBuffering stdout NoBuffering
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    watchPosts dbconf p
    Database.Persist.Store.runPool dbconf (runMigration migrateAll >> reloadDB) p
    let foundation = Milagos conf setLogger s p manager dbconf
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

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
