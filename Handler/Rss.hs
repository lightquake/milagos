module Handler.Rss where

import           Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Data.Time.Format
import           Handler.Renderers (postRouter)
import           Import
import           System.Locale
import           Text.Blaze.Renderer.Text
import           Text.Hamlet.XML
import           Text.XML
import           Yesod.Default.Config


getRssR :: Handler RepXml
getRssR = do
  public <- publicFilter
  posts <- fmap (map entityVal) . runDB $ selectList public [Desc PostId, LimitTo 20]
  blogTitle <- extraTitle . appExtra . settings <$> getYesod
  renderer <- getUrlRender
  router <- postRouter
  let description = "A Milagos blog"
  return $ buildDoc $(xmlFile "templates/rss.xhamlet")

buildDoc :: [Node] -> RepXml
buildDoc nodes = RepXml . toContent . renderText def $ doc
  where doc = Document prol (Element "rss" [("version", "2.0")
                                          , ("xmlns:atom", "http://www.w3.org/2005/Atom")
                                          ] nodes) []
        prol = Prologue [] Nothing []

rfc822 :: PostGeneric backend -> T.Text
rfc822 = T.pack . formatTime defaultTimeLocale fmtString . postPosted
  where fmtString = "%a, %0d %b %Y %H:%M:%S GMT"