module Handler.Rss where

import Control.Applicative
import Import
import Text.Hamlet.XML
import Text.XML
import Yesod.Default.Config

getRssR :: Handler RepXml
getRssR = do
  posts <- runDB $ selectList [] [Desc PostId, LimitTo 20]
  blogTitle <- extraTitle . appExtra . settings <$> getYesod
  gurp <- getUrlRenderParams
  let renderer route = gurp route []
      description = "A Milagos blog"
  return $ buildDoc $(xmlFile "templates/rss.xhamlet")

buildDoc :: [Node] -> RepXml
buildDoc nodes = RepXml . toContent . renderText def $ doc
  where doc = Document prol (Element "rss" [("version", "2.0")
                                          , ("xmlns:atom", "http://www.w3.org/2005/Atom")
                                          ] nodes) []
        prol = Prologue [] Nothing []