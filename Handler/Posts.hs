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
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle ""
    $(widgetFile "post-list")
    [whamlet|<div .pagination>^{widget}|]

-- | Latest posts with a given tag.
getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  public <- publicFilter
  (postEnts, widget) <- runDB (postsWithTag public tagText) >>= paginateWith pageWidget 6
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle $ toHtml tagText
    $(widgetFile "post-list")
    [whamlet|<div .pagination>^{widget}|]

-- | Render the unique post with a given date and slug. Logs an error
-- if there's more than one such post and renders an arbitrary one.
getPostR :: Integer -> Padded -> Padded -> Text -> Handler RepHtml
getPostR year (Padded month) (Padded day) slug = do
  postEnt <- getPost
  posts <- (:[]) <$> postWidget postEnt
  blogLayout $ do
    setTitle $ toHtml . postTitle . entityVal $ postEnt
    $(widgetFile "post-list")
  where
    getPost :: Handler (Entity Post)
    getPost = do
      tz <- liftIO getCurrentTimeZone
      let date = fromGregorian year month day
          startTime = localTimeToUTC tz $ LocalTime date midnight
          endTime = localTimeToUTC tz $ LocalTime (succ date) midnight
      posts <- runDB $ selectList [ PostPosted <. endTime
                                  , PostPosted >=. startTime
                                  , PostSlug ==. slug
                                  ] []
      let query = (T.pack . show) date <> ", " <> slug
      -- error if there's more than one post, notFound if there's none
      when (length posts > 1) $ $(logError) ("more than one post matching " <> query)
      when (null posts) notFound
      return $ head posts