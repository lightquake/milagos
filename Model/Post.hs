module Model.Post where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Query.Join
import Model
import Prelude

data FullPostGeneric backend = FullPost { fpPost :: PostGeneric backend
                                        , fpTags :: [TagGeneric backend]
                                        }

-- | Map a Post entity to its list of Tag entities
tagsFor :: PersistQuery backend m => Entity (PostGeneric backend) -> backend m [Entity (TagGeneric backend)]
tagsFor post = do
  som <- runJoin $ (selectOneMany (PostTagTagId <-.) postTagTagId)
           { somFilterMany = [PostTagPostId ==. entityKey post]
           , somOrderOne = [Asc TagName]}
  return $ map fst som

-- | Given a tag name, return the list of Post entities that have it,
-- filtered by the given filter.
postsWithTag :: (PersistUnique back m, PersistQuery back m) =>
                [Filter (PostGeneric back)] -> Text
                -> back m [Entity (PostGeneric back)]
postsWithTag filters tagText = do
  tagEnt' <- getBy $ UniqueTagName tagText
  case tagEnt' of
    Just tagEnt -> do
     som <- runJoin $ (selectOneMany (PostTagPostId <-.) postTagPostId)
       { somFilterMany = [PostTagTagId ==. entityKey tagEnt]
       , somOrderOne = [Desc PostPosted]
       , somFilterOne = filters}
     return $ map fst som
    _ -> return []