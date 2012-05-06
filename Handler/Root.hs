module Handler.Root where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Handler.Renderers
import Import
import Prelude (head)
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
  now <- liftIO getCurrentTime
  (postEnts, widget) <- runDB $
                        selectPaginatedWith pageWidget 6 [
                            PostIsDraft ==. False
                          , PostPosted <=. now
                          ] [Desc PostPosted]
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle ""
    $(widgetFile "post-list")
    [whamlet|<div .pagination>^{widget}|]

getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  now <- liftIO getCurrentTime
  let filters = [PostIsDraft ==. False, PostPosted <=. now]
  (postEnts, widget) <- runDB (postsWithTag filters tagText) >>= paginateWith pageWidget 6
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle $ toHtml tagText
    $(widgetFile "post-list")
    [whamlet|<div .pagination>^{widget}|]

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