module Model.Post where

import Database.Persist
import Database.Persist.Query.Join
import Model
import Prelude

data FullPostGeneric backend = FullPost { fpPost :: PostGeneric backend
                                        , fpTags :: [TagGeneric backend]
                                        } deriving Show

-- | Map a Post entity to its list of tag strings.
tagsFor :: PersistQuery backend m => Entity (PostGeneric backend) -> backend m [TagGeneric backend]
tagsFor post = do
  som <- runJoin $ (selectOneMany (PostTagTagId <-.) postTagTagId)
           { somFilterMany = [PostTagPostId ==. entityKey post] }
  return $ map (entityVal . fst) som

mkFullPost :: PersistQuery backend m => Entity (PostGeneric backend) -> backend m (FullPostGeneric backend)
mkFullPost postEnt = do
  tags <- tagsFor postEnt
  return $ FullPost (entityVal postEnt) tags