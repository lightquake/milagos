module Handler.Admin where

import Import
import qualified Data.Text as T

data PostF = PostF {
    title :: Text
  , body :: Text
  , tags :: [Text]
  } deriving Show

postForm :: Form PostF
postForm = renderDivs $ PostF
           <$> areq textField "Title" Nothing
           <*> unTextarea `fmap` areq textareaField "Body" Nothing
           <*> areq tagListField "Tags" Nothing


getAdminR :: Handler RepHtml
getAdminR = do
  ((_, widget), enctype) <- generateFormPost postForm
  defaultLayout [whamlet|
 <form action=@{AdminR} method=post enctype=#{enctype}
    ^{widget}
    <input type=submit>
|]

postAdminR :: Handler RepHtml
postAdminR = do
  ((result, widget), enctype) <- runFormPost postForm
  case result of
    FormSuccess postf -> do
      runDB $ mapM (insert . Tag) (tags postf) >> insert (Post (title postf) (body postf))

      redirect RootR

-- A field for a list of tags, which are comma-separated.
tagListField :: Field sub master [Text]
tagListField = Field {
    fieldParse = \vals -> return . Right . Just . map T.strip $ vals >>= T.splitOn ","
  , fieldView = \idAttr nameAttr theClass result _ -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} :not (null theClass):class="#{T.intercalate " " theClass}" type=text value=#{either (const "") (T.intercalate ", ") result}>
|] }
