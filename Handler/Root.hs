module Handler.Root where

import qualified Data.Text as T
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

-- | Turn a Post entity into a widget.
postWidget :: (PersistQuery backend Handler, backend ~ YesodPersistBackend Milagos) =>
              Entity (PostGeneric backend) -> Widget
postWidget postEnt = do
  FullPost post tagVals <- lift . runDB $ mkFullPost postEnt
  let tags = map tagName tagVals
  [whamlet|
<h1>#{postTitle post}
<div>#{preEscapedText $ postBody post}
$if not (null tags)
  <div>Tagged: #{T.intercalate ", " tags}|]