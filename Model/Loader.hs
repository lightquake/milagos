module Model.Loader where

import qualified Control.Exception.Lifted as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text.IO (readFile)
import           Data.Time
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Store
import           Import hiding (parseTime)
import           System.Directory
import           System.FilePath
import           System.INotify
import           System.Locale
import           Text.Blaze (preEscapedText)
import           Text.Discount

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
  _ <- addWatch inotify [Create] "posts" $
    \ev -> when (isDirectory ev) $ watchPost inotify (filePath ev)

  -- watch existing directories for changes
  postDirectories <- liftIO . getDirectoryContents $ "posts"
  mapM_ (watchPost inotify) $ filter (\x -> length x > 2) postDirectories
    where
      changeEvents = [Create, Delete, DeleteSelf, Modify, MoveIn, MoveOut, MoveSelf]
      watchPost :: INotify -> FilePath -> IO ()
      watchPost i dir =
        addWatch i changeEvents ("posts" </> dir) (reload dir) >> return ()
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

loadPosts :: (MonadIO (back m), PersistUnique back m) => back m ()
loadPosts = do
  postDirectories <- liftIO . getDirectoryContents $ "posts"

  -- catch exceptions so we don't die on bad posts
  let safeLoad post = loadPost post `E.catch` \e -> liftIO . putStrLn $
        "While loading " ++ post ++ " caught " ++ show (e :: E.SomeException)
  mapM_ safeLoad (filter (\x -> x `notElem` [".", ".."]) postDirectories)

loadPost :: (MonadIO (back m), PersistUnique back m)
            => FilePath -> back m ()
loadPost postFolder = do
  body <- liftIO . readFile $ "posts" </> postFolder </> "post.markdown"
  mMeta <- liftIO . decodeFile $ "posts" </> postFolder </> "meta.yml"
  let parsedBody = preEscapedText $ parseMarkdownUtf8 [] body

  case mMeta of
    Nothing -> fail "failed to parse metadata!"
    Just meta -> do
      -- get the metadata out of the meta.yml file
      (post, tags) <- parseMonad (buildPost meta) parsedBody
      tagIds <- map entityKey <$> mapM getMakeTag tags

      -- insert the Post and PostTag objects
      postId <- insert post
      mapM_ (insert . PostTag postId) tagIds


-- Utility functions
---------------------------------------------------------------------------
-- Get the title and tags out of a parsed metadata Object.
buildPost :: Object -> Html -> Parser (PostGeneric back, [Text])
buildPost o body = do
  title <- o .: "title"
  slug <- o .: "slug"
  tags <- o .:? "tags" .!= []
  posted <- readTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> o .: "posted"
  draft <- o .:? "draft" .!= False
  let post = Post { postIsDraft = draft
                  , postTitle = title
                  , postSlug = slug
                  , postBody = body
                  , postPosted = posted
                  }
  return (post, tags)

-- Given a tag name, either retrieves the tag Entity with that name
-- from the database or creates a new one and inserts it.
getMakeTag :: PersistUnique back m => Text -> back m (Entity (TagGeneric back))
getMakeTag name = do
  mTag <- getBy $ UniqueTagName name
  case mTag of
    Just tag -> return tag
    Nothing -> do
      tagId <- insert $ Tag name
      return $ Entity tagId (Tag name)

