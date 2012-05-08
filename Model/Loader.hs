module Model.Loader where

import           Control.Monad
import           Control.Monad.IO.Class
import           Database.Persist
import           Database.Persist.Store
import           Import hiding (parseTime)
import           Model.Loader.Post
import           System.Directory
import           System.FilePath
import           System.INotify

-- | Watch all the posts in the posts subdirectory. Takes the db
-- configuration and pool as an option so we can generate IO actions
-- to pass to the inotify watcher.
watchPosts :: (MonadIO (back IO), PersistConfig c,
               PersistUnique back IO,
               PersistQuery back IO,
               back ~ PersistConfigBackend c) =>
              c -> PersistConfigPool c -> IO ()
watchPosts dbconf pool = do
  inotify <- initINotify
  -- set up watcher to create new watchers as new directories are
  -- created
  void . addWatch inotify [Create] "posts" $
    \ev -> when (isDirectory ev) $ watchPost inotify (filePath ev) >> runPool dbconf reloadDB pool

  -- watch existing directories for changes
  postDirectories <- liftIO . getDirectoryContents $ "posts"
  mapM_ (watchPost inotify) $ filter (`notElem` [".", ".."]) postDirectories
    where
      changeEvents = [Create, Delete, DeleteSelf, Modify, MoveIn, MoveOut, MoveSelf]
      watchPost :: INotify -> FilePath -> IO ()
      watchPost i dir = void $ addWatch i changeEvents ("posts" </> dir) (reload dir)
      reload :: FilePath -> Event -> IO ()
      reload dir ev = do
        putStrLn $ "Caught an event " ++ show ev ++ ", in " ++ dir ++ ", reloading"
        runPool dbconf reloadDB pool



-- Actions that related to reloading posts.
--------------------------------------------------------

-- | Remove all the posts, then repopulate the database from "posts/"
reloadDB :: (PersistQuery back m, PersistUnique back m, MonadIO (back m)) => back m ()
reloadDB = do
  deleteWhere ([] :: [Filter Post])
  deleteWhere ([] :: [Filter Tag])
  deleteWhere ([] :: [Filter PostTag])
  loadPosts

