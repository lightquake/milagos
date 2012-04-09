module Model.Tag where

import Control.Applicative ((<$>))
import Database.Persist
import Database.Persist.Query.Join
import Model
import Prelude

tagsInUse :: PersistQuery b m => b m [Entity (TagGeneric b)]
tagsInUse = do
  let som = (selectOneMany (PostTagTagId <-.) postTagTagId) {
          somIncludeNoMatch = False
        , somOrderOne = [Asc TagName]}
  map fst <$> runJoin som
