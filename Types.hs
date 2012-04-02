module Types
  ( Milagos (Milagos, settings, getLogger, getStatic, connPool, httpManager, persistConfig)
  , FullPost
  ) where

import           Database.Persist.GenericSql.Raw
import qualified Database.Persist.Store
import           Model.Post
import           Network.HTTP.Conduit (Manager)
import           Prelude
import qualified Settings
import           Settings (Extra (..))
import           Yesod.Default.Config
import           Yesod.Logger (Logger)
import           Yesod.Static

data Milagos = Milagos
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

type FullPost = FullPostGeneric SqlPersist