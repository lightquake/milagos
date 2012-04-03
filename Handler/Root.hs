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
  fullPosts <- runDB $ do
    posts <- selectList [] [Desc PostId]
    mapM mkFullPost posts
  let posts = map postWidget fullPosts
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  posts <- map postWidget <$> (runDB $ postsWithTag tagText)
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

-- | Turn a FullPost entity into a widget.
postWidget :: FullPostGeneric backend -> Widget
postWidget (FullPost post tagVals) = do
  let tags = map tagName tagVals
  [whamlet|
<h1>#{postTitle post}
<div>#{preEscapedText $ postBody post}
$if not (null tags)
  <div>Tagged: #{T.intercalate ", " tags}|]


getPostR :: PostId -> Handler RepHtml
getPostR postId = do
  fullPost <- runDB $ do
    post <- get404 postId
    mkFullPost $ Entity postId post
  let posts = [postWidget fullPost]
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")
