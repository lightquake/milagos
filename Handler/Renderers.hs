{-# LANGUAGE RecordWildCards #-}
module Handler.Renderers where

import           Control.Monad
import           Data.Time
import           Import hiding (parseTime)
import           Prelude (head, tail)
import           System.Locale
import           Yesod.Default.Config
import           Yesod.Paginator


-- | Get a function that turns Posts into Routes. Useful for putting
-- in Hamlet templates.
postRouter :: Handler (PostGeneric backend -> Route Milagos)
postRouter = liftIO $ do
  tz <- getCurrentTimeZone
  return $ \(Post{..}) ->
    let (year, month, day) = toGregorian . localDay . utcToLocalTime tz $ postPosted
        in PostR year (Padded month) (Padded day) postSlug


-- | Render a single post into a Widget.
postWidget :: Entity Post -> Handler Widget
postWidget postEnt = do
  let Entity postKey post = postEnt
  tags <- runDB $ map (tagName . entityVal) <$> tagsFor postEnt
  localTime <- liftIO . utcToLocalZonedTime $ postPosted post
  -- like April 11, 2012
  let time = formatTime defaultTimeLocale "%B %e, %Y" localTime
  router <- postRouter
  return $(widgetFile "post")

-- | The tag list sidebar Widget.
tagListWidget :: Widget
tagListWidget = do
  tags <- lift . runDB $ map (tagName . entityVal) <$> tagsInUse
  $(widgetFile "tag-list")

-- | Lay out a Widget in the standard blog layout, with a sidebar,
-- footer, etc.
blogLayout :: Widget -> Handler RepHtml
blogLayout widget = do
  ae <- appExtra . settings <$> getYesod
  mmsg <- getMessage
  let blogTitle = extraTitle ae
      mAnalytics = extraAnalytics ae
  let footer = $(widgetFile "footer")
  defaultLayout $(widgetFile "blog-layout")

-- | A pagination widget for paging back and forth between newer/older
-- contents. Uses the p parameter in the query string to keep track of
-- what page is current.
pageWidget :: Int -> Int -> Int -> GWidget sub master ()
pageWidget = paginationWidget $ PageWidgetConfig {
    prevText = "Newer"
  , nextText = "Older"
  , pageCount = 2
  , ascending = True
  , showEllipsis = True
  }