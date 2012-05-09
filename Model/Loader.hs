module Model.Loader where

import Control.Monad
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Store
import Import hiding (parseTime)
import Model.Loader.Page
import Model.Loader.Post
import System.Directory
import System.FilePath
import System.INotify


-- | Watch all the posts in the posts subdirectory. Takes the db
-- configuration and pool as an option so we can generate IO actions
-- to pass to the inotify watcher.
watchPosts, watchPages :: (MonadIO (back IO), PersistConfig c,
               PersistUnique back IO,
               PersistQuery back IO,
               back ~ PersistConfigBackend c) =>
              c -> PersistConfigPool c -> IO ()
watchPosts dbconf pool = watchDir "posts/" $ runPool dbconf reloadPosts pool
watchPages dbconf pool = watchDir "pages/" $ runPool dbconf reloadPages pool

watchDir :: FilePath -> IO () -> IO ()
watchDir mainDir reloader = do
  inotify <- initINotify
  -- set up watcher to create new watchers as new directories are
  -- created
  void . addWatch inotify [Create] mainDir $
    \ev -> when (isDirectory ev) $ watchPost inotify (filePath ev) >> reloader

  -- watch existing directories for changes
  postDirectories <- liftIO . getDirectoryContents $ mainDir
  mapM_ (watchPost inotify) $ filter (`notElem` [".", ".."]) postDirectories
    where
      changeEvents = [Create, Delete, DeleteSelf, Modify, MoveIn, MoveOut, MoveSelf]
      watchPost :: INotify -> FilePath -> IO ()
      watchPost i dir = void $ addWatch i changeEvents (mainDir </> dir) (reload dir)
      reload :: FilePath -> Event -> IO ()
      reload dir ev = do
        putStrLn $ "Caught an event " ++ show ev ++ ", in " ++ dir ++ ", reloading"
        reloader
