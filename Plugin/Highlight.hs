module Plugin.Highlight where

import Data.Text.Lazy (Text)
import Data.Yaml
import Prelude
import Text.Blaze (preEscapedLazyText, Html)
import Text.Blaze.Renderer.Text (renderHtml)
import Text.HTML.TagSoup

-- | If the object has a code-language text property, then add that
-- property as a class to all <code> blocks. Otherwise, do nothing.
addCodeClasses :: Object -> Html -> Parser Html
addCodeClasses o html = do
  -- get out the code-language attribute
  mlang <- (o .:? "code-language") :: Parser (Maybe Text)
  case mlang of
    -- no attribute returns it unchanged
    Nothing -> return html
    Just lang -> return $ preEscapedLazyText . renderTags . map (addClass lang) $ soup
  where
    soup :: [Tag Text]
    soup = parseTags . renderHtml $ html
    addClass :: Text -> Tag Text -> Tag Text
    addClass cls (TagOpen "code" attrs) = TagOpen "code" $
                                          ("class", cls) : filter ((/= "class") . fst) attrs
    addClass _  tag = tag
