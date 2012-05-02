module Types
  ( Milagos (Milagos, settings, getLogger, getStatic, connPool, httpManager, persistConfig)
  , FullPost
  , Padded(..)
  ) where

import qualified Data.Text as T
import           Database.Persist.GenericSql.Raw
import qualified Database.Persist.Store
import           Model.Post
import           Network.HTTP.Conduit (Manager)
import           Prelude
import qualified Settings
import           Settings (Extra (..))
import           Yesod (PathPiece(..))
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

newtype Padded = Padded Int deriving (Eq, Ord, Read)
instance Show Padded where
  show (Padded x) | x < 10 = '0' : show x
                  | otherwise = show x

instance PathPiece Padded where
  fromPathPiece = fmap Padded . fromPathPiece
  toPathPiece = T.pack . show