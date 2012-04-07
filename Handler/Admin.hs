module Handler.Admin where

import Import
import qualified Data.Text as T

data PostF = PostF {
    title :: Text
  , body :: Text
  , tags :: [Text]
  } deriving Show

postForm :: Maybe PostF -> Form PostF
postForm postf = renderDivs $ PostF
           <$> areq textField "Title" (title <$> postf)
           <*> unTextarea `fmap` areq textareaField "Body" (Textarea . body <$> postf)
           <*> areq tagListField "Tags" (tags <$> postf)


getAdminR :: Handler RepHtml
getAdminR = do
  ((_, widget), enctype) <- generateFormPost $ postForm Nothing
  defaultLayout [whamlet|
 <form action=@{AdminR} method=post enctype=#{enctype}
    ^{widget}
    <input type=submit>
|]

postAdminR :: Handler RepHtml
postAdminR = do
  result <- fst . fst <$> runFormPost (postForm Nothing)
  case result of
    FormSuccess postf -> do
      _ <- runDB $ do
        postId <- insert (Post (title postf) (body postf))
        -- get the keys and insert into the PostTag many-many table
        tagKeys <- loadTagKeys (tags postf)
        mapM (insert . PostTag postId) tagKeys
      redirect RootR
    _ -> redirect AdminR

getEditPostR :: PostId -> Handler RepHtml
getEditPostR postId = do
  postF <- runDB $ mkPostF postId
  ((_, widget), enctype) <- generateFormPost . postForm $ Just postF
  defaultLayout [whamlet|
 <form action=@{EditPostR postId} method=post enctype=#{enctype}
    ^{widget}
    <input type=submit>
|]

postEditPostR :: PostId -> Handler RepHtml
postEditPostR postId = do
  newPost <- fst . fst <$> runFormPost (postForm Nothing)
  case newPost of
    FormSuccess postf -> do
      _ <- runDB $ do
        replace postId $ Post (title postf) (body postf)
        -- out with the old tags, in with the new ones
        tagKeys <- loadTagKeys (tags postf)
        deleteWhere [PostTagPostId ==. postId]
        mapM (insert . PostTag postId) tagKeys

      redirect $ PostR postId
    _ -> redirect $ EditPostR postId

-- A field for a list of tags, which are comma-separated.
tagListField :: Field sub master [Text]
tagListField = Field {
    fieldParse = \vals -> return . Right . Just . filter (not . T.null) . map T.strip $ vals >>= T.splitOn ","
  , fieldView = \idAttr nameAttr theClass result _ -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} :not (null theClass):class="#{T.intercalate " " theClass}" type=text value=#{either (const "") (T.intercalate ", ") result}>
|] }

-- | Given a list of Texts that correspond to tag names, return the
-- Keys for the corresponding tags. Creates the tags if necessary.
loadTagKeys :: PersistUnique backend m => [Text] -> backend m [Key backend (TagGeneric backend)]
loadTagKeys = mapM loadTag
 where loadTag tag = do
         maybeTagEnt <- getBy $ UniqueTagName tag
         case maybeTagEnt of
           Just tagEnt -> return $ entityKey tagEnt
           Nothing -> insert $ Tag tag

mkPostF :: PostId -> YesodDB Milagos Milagos PostF
mkPostF postId = do
  postVal <- get404 postId
  let postEnt = Entity postId postVal
  tagNames <- map (tagName . entityVal) <$> tagsFor postEnt
  return $ PostF (postTitle postVal) (postBody postVal) tagNames