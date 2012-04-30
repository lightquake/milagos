{-# LANGUAGE ScopedTypeVariables #-}
module Model.Loader where

import qualified Control.Exception.Lifted as E
import           Control.Monad.IO.Class
import           Data.List.Split
import           Data.Text.IO (readFile)
import           Data.Time
import           Data.Yaml
import           Database.Persist
import           Import
import           System.Directory
import           System.FilePath
import           Text.Blaze (preEscapedText)
import           Text.Discount

-- | Clear the database of posts so they can be loaded again.
clearDB :: PersistQuery back m => back m ()
clearDB = do
  deleteWhere ([] :: [Filter Post])
  deleteWhere ([] :: [Filter Tag])
  deleteWhere ([] :: [Filter PostTag])

loadPosts :: (MonadIO (back m), PersistUnique back m) => back m ()
loadPosts = do
  postDirectories <- liftIO . getDirectoryContents $ "posts"
  -- filter out "." and ".."
  let safeLoad post = loadPost post `E.catch` \e -> liftIO . putStrLn $
        "While loading " ++ post ++ " caught " ++ show (e :: E.SomeException)
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
      (title, tags) <- parseMonad postData meta
      tagIds <- map entityKey <$> mapM getMakeTag tags
      time <- liftIO getPostTime

      -- insert the Post and PostTag objects
      postId <- insert $ Post title parsedBody time
      mapM_ (insert . PostTag postId) tagIds

  where (year:month:day:_) = splitOn "-" postFolder
        getPostTime :: IO UTCTime
        getPostTime = do
          tz <- getCurrentTimeZone
          -- we can't map read here because the first argument is an
          -- Integer, but the others are Ints
          let date = fromGregorian (read year) (read month) (read day)
          return $ localTimeToUTC tz $ LocalTime date midnight


postData :: Object -> Parser (Text, [Text])
postData o = do
  title <- o .: "title"
  tags <- o .:? "tags" .!= []
  return (title, tags)


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