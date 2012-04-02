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
  result <- fst . fst <$> runFormPost postForm
  case result of
    FormSuccess postf -> do
      _ <- runDB $ do
        postId <- insert (Post (title postf) (body postf))
        -- get the keys and insert into the PostTag many-many table
        tagKeys <- loadTags (tags postf)
        mapM (insert . PostTag postId) tagKeys
      redirect RootR
    _ -> redirect AdminR

-- A field for a list of tags, which are comma-separated.
tagListField :: Field sub master [Text]
tagListField = Field {
    fieldParse = \vals -> return . Right . Just . filter (not . T.null) . map T.strip $ vals >>= T.splitOn ","
  , fieldView = \idAttr nameAttr theClass result _ -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} :not (null theClass):class="#{T.intercalate " " theClass}" type=text value=#{either (const "") (T.intercalate ", ") result}>
|] }

-- | Given a list of Texts that correspond to tag names, return the
-- Keys for the corresponding tags. Creates the tags if necessary.
loadTags :: PersistUnique backend m => [Text] -> backend m [Key backend (TagGeneric backend)]
loadTags = mapM loadTag
 where loadTag tag = do
         maybeTagEnt <- getBy $ UniqueTagName tag
         case maybeTagEnt of
           Just tagEnt -> return $ entityKey tagEnt
           Nothing -> insert $ Tag tag
