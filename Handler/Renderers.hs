module Handler.Renderers where

import Import
import Prelude (head, tail)
import Yesod.Default.Config
import Text.Blaze (preEscapedText)

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