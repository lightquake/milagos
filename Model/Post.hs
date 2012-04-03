module Model.Post where

import           Data.Text (Text)
import           Database.Persist
import           Database.Persist.Query.Join
import           Model
import           Prelude

data FullPostGeneric backend = FullPost { fpPost :: PostGeneric backend
                                        , fpTags :: [TagGeneric backend]
                                        } deriving Show

-- | Map a Post entity to its list of Tag entities
tagsFor :: PersistQuery backend m => Entity (PostGeneric backend) -> backend m [Entity (TagGeneric backend)]
tagsFor post = do
  som <- runJoin $ (selectOneMany (PostTagTagId <-.) postTagTagId)
           { somFilterMany = [PostTagPostId ==. entityKey post] }
  return $ map fst som

-- | Given a tag name, return the list of Post entities that have it.
postsWithTag :: (PersistUnique backend m, PersistQuery backend m) =>
                Text -> backend m [Entity (PostGeneric backend)]
postsWithTag tagText = do
  tagEnt' <- getBy $ UniqueTagName tagText
  case tagEnt' of
    Just tagEnt -> do
     som <- runJoin $ (selectOneMany (PostTagPostId <-.) postTagPostId)
       {  somFilterMany = [PostTagTagId ==. entityKey tagEnt] }
     return $ map fst som
    _ -> return []