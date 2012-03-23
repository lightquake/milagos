module Types
  ( Milagos (Milagos, settings, getLogger, getStatic, connPool, httpManager, persistConfig)
  ) where

import Prelude
import Yesod.Static
import Yesod.Default.Config
import Yesod.Logger (Logger)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings (Extra (..))


data Milagos = Milagos
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }
