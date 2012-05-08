module Handler.Posts where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Handler.Renderers
import Import
import Prelude (head)
import Yesod.Paginator

-- | Handle the root page; i.e., the latest posts.
getRootR :: Handler RepHtml
getRootR = do
  public <- publicFilter
  (postEnts, widget) <- runDB $ selectPaginatedWith pageWidget 6 public [Desc PostPosted]
  showPosts "" postEnts (Just widget)

-- | Latest posts with a given tag.
getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  public <- publicFilter
  (postEnts, widget) <- runDB (postsWithTag public tagText) >>= paginateWith pageWidget 6
  showPosts tagText postEnts (Just widget)

-- | Render the unique post with a given date and slug. Logs an error
-- if there's more than one such post and renders an arbitrary one.
getPostR :: Integer -> Padded -> Padded -> Text -> Handler RepHtml
getPostR year (Padded month) (Padded day) slug = do
  let query = (T.pack . show) date <> ", " <> slug
      date = fromGregorian year month day
  postEnts <- runDB $ postsFromDateSlug (fromGregorian year month day) slug
  -- error if there's more than one post, notFound if there's none
  when (length postEnts > 1) $ $(logError) ("more than one post matching " <> query)
  when (null postEnts) notFound
  let postEnt = head postEnts
  showPosts (postTitle . entityVal $ postEnt) [postEnt] Nothing

showPosts :: Text -> [Entity Post] -> Maybe Widget -> Handler RepHtml
showPosts title postEnts mwidget = do
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle $ toHtml title
    $(widgetFile "post-list")
    case mwidget of
      Just widget -> [whamlet|<div .pagination>^{widget}|]
      Nothing -> return ()

getDraftsR :: Handler RepHtml
getDraftsR = do
  (postEnts, widget) <- runDB $ selectPaginatedWith pageWidget 6 [PostIsDraft ==. True] [Desc PostPosted]
  showPosts "Drafts" postEnts (Just widget)