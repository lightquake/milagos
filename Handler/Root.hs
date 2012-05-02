module Handler.Root where

import Control.Monad
import Data.Time
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
  (postEnts, widget) <- runDB $ selectPaginatedWith pageWidget 6 [PostIsDraft ==. False] [Desc PostPosted]
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle ""
    $(widgetFile "post-list")
    [whamlet|<div .pagination>^{widget}|]

getTagR :: Text -> Handler RepHtml
getTagR tagText = do
  (postEnts, widget) <- (runDB $ postsWithTag [PostIsDraft ==. False] tagText) >>= paginateWith pageWidget 6
  posts <- mapM postWidget postEnts
  blogLayout $ do
    setTitle $ toHtml tagText
    $(widgetFile "post-list")
    [whamlet|<div .pagination>^{widget}|]

getPostR :: Integer -> Padded -> Padded -> Text -> Handler RepHtml
getPostR year (Padded month) (Padded day) slug = do
  tz <- liftIO getCurrentTimeZone
  let date = fromGregorian year month day
      utcTime = localTimeToUTC tz $ LocalTime date midnight
  postEnt <- runDB . getBy404 $ UniqueTimeSlug utcTime slug
  posts <- (:[]) <$> postWidget postEnt
  blogLayout $ do
    setTitle $ toHtml . postTitle . entityVal $ postEnt
    $(widgetFile "post-list")


