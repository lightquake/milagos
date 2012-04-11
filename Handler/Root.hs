module Handler.Root where

import Control.Monad
import Handler.Renderers
import Import
import Yesod.Paginator

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  (postEnts, widget) <- runDB $ selectPaginated pageWidget 6 [] [Desc PostPosted]
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle ""
    $(widgetFile "post-list")
    widget

getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  (postEnts, widget) <- (runDB $ postsWithTag tagText) >>= paginate pageWidget 6
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle $ toHtml tagText
    $(widgetFile "post-list")
    widget

getPostR :: PostId -> Handler RepHtml
getPostR postId = do
  postVal <- runDB $ get404 postId
  posts <- (:[]) <$> postWidget (Entity postId postVal)
  blogLayout $ do
    setTitle $ toHtml . postTitle $ postVal
    $(widgetFile "post-list")


