module Model.Loader.Page (reloadPages) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text (pack)
import Data.Text.IO (readFile)
import Data.Yaml
import Import
import Model.Loader.Util
import System.FilePath
import Text.Blaze
import Text.Discount

reloadPages :: (PersistQuery back m, PersistUnique back m, MonadIO (back m)) => back m ()
reloadPages = do
  deleteWhere ([] :: [Filter StaticPage])
  loadPages

loadPages :: (MonadIO (back m), PersistUnique back m) => back m ()
loadPages = loadMulti loadPage "pages"

loadPage :: (MonadIO (back m), PersistUnique back m)
            => FilePath -> back m ()
loadPage dir = do
  body <- liftIO . readFile $ dir </> "page.markdown"
  mMeta <- liftIO . decodeFile $ dir </> "meta.yml"
  let parsedBody = preEscapedText $ parseMarkdownUtf8 [] body

  case mMeta of
    Nothing -> fail "failed to parse metadata!"
    Just meta -> do
      title <- parseMonad (.: "title") meta
      void . insert $ StaticPage (pack $ takeBaseName dir) title parsedBody