module Handler.Renderers where

import Import
import Text.Blaze
import Prelude (head, tail)

postWidget :: Entity Post -> Handler Widget
postWidget postEnt = runDB $ do
  let post = entityVal postEnt
  tags <- map (tagName . entityVal) <$> tagsFor postEnt
  return $ $(whamletFile "templates/post.hamlet")