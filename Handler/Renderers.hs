module Handler.Renderers where

import qualified Data.Text as T
import           Import
import           Text.Blaze

postWidget :: Entity Post -> Handler Widget
postWidget postEnt = runDB $ do
  let post = entityVal postEnt
  tags <- map (tagName . entityVal) <$> tagsFor postEnt
  return $ $(whamletFile "templates/post.hamlet")