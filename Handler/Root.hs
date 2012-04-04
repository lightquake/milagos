module Handler.Root where

import           Control.Monad
import           Handler.Renderers
import           Import

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  postEnts <- runDB $ selectList [] [Desc PostId]
  posts <- mapM postWidget postEnts
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "postList")

getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  postEnts <- runDB $ postsWithTag tagText
  posts <- mapM postWidget postEnts
  defaultLayout $ do
    setTitle $ toHtml tagText
    $(widgetFile "postList")

getPostR :: PostId -> Handler RepHtml
getPostR postId = do
  postVal <- runDB $ get404 postId
  posts <- (:[]) <$> postWidget (Entity postId postVal)
  defaultLayout $ do
    setTitle $ toHtml . postTitle $ postVal
    $(widgetFile "postList")