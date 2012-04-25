module Handler.Renderers where

import           Control.Monad
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Import hiding (parseTime)
import           Prelude (head, tail)
import           System.Locale
import           Yesod.Auth
import           Yesod.Default.Config
import           Yesod.Paginator

postWidget :: Entity Post -> Handler Widget
postWidget postEnt = do
  let Entity postKey post = postEnt
  tags <- runDB $ map (tagName . entityVal) <$> tagsFor postEnt
  localTime <- liftIO . utcToLocalZonedTime $ postPosted post
  -- like April 11, 2012
  let time = formatTime defaultTimeLocale "%B %e, %Y" localTime
  authorized <- (== Authorized) <$> isAuthorized (EditPostR postKey) True
  return $(widgetFile "post")

tagListWidget :: Widget
tagListWidget = do
  tags <- lift . runDB $ map (tagName . entityVal) <$> tagsInUse
  $(widgetFile "tag-list")

blogLayout :: Widget -> Handler RepHtml
blogLayout widget = do
  ae <- appExtra . settings <$> getYesod
  mmsg <- getMessage
  loggedIn <- isJust <$> maybeAuthId
  let blogTitle = extraTitle ae
      mAnalytics = if loggedIn then Nothing else extraAnalytics ae
  defaultLayout $(widgetFile "blog-layout")

adminLayout :: Widget -> Handler RepHtml
adminLayout widget =
  defaultLayout $ do
    addStylesheet . StaticR $ StaticRoute ["css", "bootstrap.css"] []
    $(widgetFile "admin-layout")


-- Stuff for previous/next page links.
-----------------------------------------------

pageWidget :: Int -> Int -> Int -> GWidget sub master ()
pageWidget = paginationWidget $ PageWidgetConfig {
    prevText = "Newer"
  , nextText = "Older"
  , pageCount = 2
  , ascending = True
  , showEllipsis = True
  }