module Handler.Renderers where

import           Control.Monad
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time
import           Import hiding (parseTime)
import           Prelude (head, tail)
import           System.Locale
import           Yesod.Default.Config

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
  master <- getYesod
  mmsg <- getMessage
  let blogTitle = extraTitle . appExtra . settings $ master
  defaultLayout $(widgetFile "blog-layout")

adminLayout :: Widget -> Handler RepHtml
adminLayout widget =
  defaultLayout $ do
    addStylesheet . StaticR $ StaticRoute ["css", "bootstrap.css"] []
    $(widgetFile "admin-layout")


-- Stuff for previous/next page links.
-----------------------------------------------

pageWidget :: Int -> Int -> Int -> GWidget sub master ()
pageWidget page perPage total = when (pages > 1) $(widgetFile "navigation")
    where pages = (\(n, r) -> n + min r 1) $ total `divMod` perPage
          showNext = page < pages



-- | A widget that links to the previous page. Is invisible if the
-- current page is not at least 1.
prevPage :: GWidget sub master ()
prevPage = do
  page <- lift $ getCurrentPage
  prevUrl <- lift $ setParam "p" (T.pack . show $ page - 1)
  when (page > 1) [whamlet|<a href=#{prevUrl}>Newer</a>|]

-- | A widget that links to the next page. Is invisible if the current
-- page is negative (invalid).
nextPage :: GWidget sub master ()
nextPage = do
  page <- lift $ getCurrentPage
  prevUrl <- lift $ setParam "p" (T.pack . show $ page + 1)
  when (page > 0) [whamlet|<a href=#{prevUrl}>Older</a>|]


setParam :: Text -> Text -> GHandler sub master Text
setParam k v = do
  renderer' <- getUrlRenderParams
  rtm <- getRouteToMaster
  route <- liftM rtm <$> getCurrentRoute
  let renderer = renderer' (fromJust route)
  params <- reqGetParams <$> getRequest
  return . renderer $ (k, v) : (filter ((/=) k . fst) params)

-- | from pbrisbin's yesod-paginator. looks up the current page, or 1
-- on failure (since an invalid page should be treated like the first
-- one)
getCurrentPage :: GHandler s m Int
getCurrentPage = fmap (fromMaybe 1 . go) $ lookupGetParam "p"
    where
      go :: Maybe Text -> Maybe Int
      go mp = readIntegral . T.unpack =<< mp