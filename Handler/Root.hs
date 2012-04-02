module Handler.Root where

import qualified Data.Text as T
import Database.Persist.Query.Join
import Import
import Text.Blaze

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  posts' <- runDB $ selectList [] [Desc PostId]
  let posts = map postWidget posts'
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

-- | Map a Post entity to its list of tag strings.
tagsFor :: PersistQuery backend m => Entity (PostGeneric backend) -> backend m [Text]
tagsFor post = do
  som <- runJoin $ (selectOneMany (PostTagTagId <-.) postTagTagId)
           { somFilterMany = [PostTagPostId ==. entityKey post] }
  return $ map (tagName . entityVal . fst) som

-- | Turn a Post entity into a widget.
postWidget :: (PersistQuery backend Handler, backend ~ YesodPersistBackend Milagos) =>
              Entity (PostGeneric backend) -> Widget
postWidget postEnt = do
  let post = entityVal postEnt
  tags <- lift . runDB $ tagsFor postEnt
  liftIO $ print (length tags)
  [whamlet|
<h1>#{postTitle post}
<div>#{preEscapedText $ postBody post}
$if not (null tags)
  <div>Tagged: #{T.intercalate ", " tags}|]