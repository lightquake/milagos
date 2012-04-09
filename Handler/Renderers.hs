module Handler.Renderers where

import           Control.Monad
import           Data.Maybe
import qualified Data.Text as T
import           Import
import           Prelude (head, tail)
import           Text.Blaze (preEscapedText)
import           Yesod.Default.Config

postWidget :: Entity Post -> Handler Widget
postWidget postEnt = do
  let Entity postKey post = postEnt
  tags <- runDB (map (tagName . entityVal) <$> tagsFor postEnt)
  authorized <- (== Authorized) <$> isAuthorized (EditPostR postKey) True
  return $(widgetFile "post")

tagListWidget :: Widget
tagListWidget = do
  tags <- lift . runDB $ (map (tagName . entityVal) <$> selectList [] [Asc TagName])
  $(widgetFile "tag-list")

blogLayout :: Widget -> Handler RepHtml
blogLayout widget = do
  master <- getYesod
  mmsg <- getMessage
  let blogTitle = extraTitle . appExtra . settings $ master
  defaultLayout $(widgetFile "blog-layout")

-- Stuff for previous/next page links.
-----------------------------------------------

pageWidget :: Int -> Int -> Int -> GWidget master master ()
pageWidget page perPage total =
  -- total number of pages is total / per + 1 (or + 0 if it's an even
  -- division)
  let pages = (\(n, r) -> n + min r 1) $ total `divMod` perPage in
    when (pages > 1) $ do
      [whamlet|
<div .navigation>
  <div .prev>^{prevPage}
  $if page < pages
    <div .next>^{nextPage}
|]

-- | A widget that links to the previous page. Is invisible if the
-- current page is not at least 1.
prevPage :: GWidget master master ()
prevPage = do
  page <- lift $ getCurrentPage
  prevUrl <- lift $ setParam "p" (T.pack . show $ page - 1)
  when (page > 1) [whamlet|<a href=#{prevUrl}>Newer</a>|]

-- | A widget that links to the next page. Is invisible if the current
-- page is negative (invalid).
nextPage :: GWidget master master ()
nextPage = do
  page <- lift $ getCurrentPage
  prevUrl <- lift $ setParam "p" (T.pack . show $ page + 1)
  when (page > 0) [whamlet|<a href=#{prevUrl}>Older</a>|]


setParam :: Text -> Text -> GHandler m m Text
setParam k v = do
  renderer' <- getUrlRenderParams
  route <- getCurrentRoute
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