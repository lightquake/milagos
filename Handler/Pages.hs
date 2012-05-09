module Handler.Pages where

import Control.Monad
import Handler.Renderers
import Import

getPageR :: Text -> Handler RepHtml
getPageR slug = do
  page <- liftM entityVal . runDB . getBy404 $ PageSlug slug
  blogLayout $ $(widgetFile "page")
