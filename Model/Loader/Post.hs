module Model.Loader.Post where

import qualified Control.Exception.Lifted as E
import           Control.Monad.IO.Class
import           Data.Text.IO (readFile)
import           Data.Time
import           Data.Yaml
import           Import
import           Model.Loader.Util
import           Plugin.Highlight
import           System.Directory
import           System.FilePath
import           Text.Blaze (preEscapedText)
import           Text.Discount


-- | Remove all the posts, then repopulate the database from "posts/"
reloadPosts :: (PersistQuery back m, PersistUnique back m, MonadIO (back m)) => back m ()
reloadPosts = do
  deleteWhere ([] :: [Filter Post])
  deleteWhere ([] :: [Filter Tag])
  deleteWhere ([] :: [Filter PostTag])
  loadPosts

-- | Load every posts from posts/ into the database.
loadPosts :: (MonadIO (back m), PersistUnique back m) => back m ()
loadPosts = loadMulti loadPost "posts"

-- | Load a single post from disk into database, adding all the tags
-- as necessary.
loadPost :: (MonadIO (back m), PersistUnique back m)
            => FilePath -> back m ()
loadPost dir = do
  body <- liftIO . readFile $ dir </> "post.markdown"
  mMeta <- liftIO . decodeFile $ dir </> "meta.yml"
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
-- | Get the title and tags out of a parsed metadata Object.
buildPost :: Object -> Html -> Parser (PostGeneric backend, [Text])
buildPost o body = do
  title <- o .: "title"
  slug <- o .: "slug"
  tags <- o .:? "tags" .!= []
  posted <- zonedTimeToUTC . read <$> o .: "posted"
  draft <- o .:? "draft" .!= False
  highlighted <- addCodeClasses o body
  let post = Post { postIsDraft = draft
                  , postTitle = title
                  , postSlug = slug
                  , postBody = highlighted
                  , postPosted = posted
                  }
  return (post, tags)

-- | Given a tag name, either retrieves the tag Entity with that name
-- from the database or creates a new one and inserts it.
getMakeTag :: PersistUnique back m => Text -> back m (Entity (TagGeneric back))
getMakeTag name = do
  mTag <- getBy $ UniqueTagName name
  case mTag of
    Just tag -> return tag
    Nothing -> do
      tagId <- insert $ Tag name
      return $ Entity tagId (Tag name)
