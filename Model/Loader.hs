module Model.Loader where

import qualified Control.Exception.Lifted as E
import           Control.Monad.IO.Class
import           Data.List.Split
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Data.Time
import           Data.Yaml
import           Database.Persist
import           Database.Persist.Store
import           Import
import           System.Directory
import           System.FilePath
import           System.INotify
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
  addWatch inotify [Create] "posts" $ watchPost inotify . filePath
  postDirectories <- liftIO . getDirectoryContents $ "posts"
  mapM_ (watchPost inotify) $ filter (\x -> length x > 2) postDirectories
    where
      changeEvents = [Create, Delete, DeleteSelf, Modify, MoveIn, MoveOut, MoveSelf]
      watchPost :: INotify -> FilePath -> IO ()
      watchPost i dir =
        addWatch i changeEvents ("posts" </> dir) (reload dir) >> return ()
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
  -- filter out "." and ".."
  mapM_ safeLoad (filter (\x -> length x > 2) postDirectories)

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
      (title, tags, draft) <- parseMonad postData meta
      tagIds <- map entityKey <$> mapM getMakeTag tags
      time <- liftIO getPostTime

      -- insert the Post and PostTag objects
      postId <- insert $ Post draft slug title parsedBody time
      mapM_ (insert . PostTag postId) tagIds

  where (year:month:day:_) = splitOn "-" postFolder
        slug = T.pack $ drop 11 postFolder
        date = fromGregorian (read year) (read month) (read day)
        getPostTime :: IO UTCTime
        getPostTime = do
          tz <- getCurrentTimeZone
          -- we can't map read here because the first argument is an
          -- Integer, but the others are Ints
          return $ localTimeToUTC tz $ LocalTime date midnight

--



-- Utility functions
---------------------------------------------------------------------------

-- Get the title and tags out of a parsed metadata Object.
postData :: Object -> Parser (Text, [Text], Bool)
postData o = do
  title <- o .: "title"
  tags <- o .:? "tags" .!= []
  draft <- o .:? "draft" .!= False
  return (title, tags, draft)


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

