module Model.Tag where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Query.Join
import Model
import Model.Post
import Prelude


-- | List all tags that are on posts that are publicly viewable (i.e.,
-- are not drafts and have post dates in the past).
tagsInUse :: (PersistQuery back m, MonadIO (back m)) =>
             back m [Entity (TagGeneric back)]
tagsInUse = do
  public <- publicFilter
  publicIds <- map entityKey <$> selectList public []
  let tagQuery = (selectOneMany (PostTagTagId <-.) postTagTagId) {
          somIncludeNoMatch = False
        , somOrderOne = [Asc TagName]
        , somFilterMany = [PostTagPostId <-. publicIds]
        }
  map fst <$> runJoin tagQuery
