module Handler.Pages where

import Handler.Renderers
import Import

getPageR :: Text -> Handler RepHtml
getPageR slug = do
  page <- runDB . getBy404 $ PageSlug slug
  blogLayout . toWidget . staticPageBody . entityVal $ page
