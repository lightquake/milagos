module Handler.Admin where

import qualified Data.Text as T
import           Handler.Renderers
import           Import

data PostF = PostF {
    title :: Text
  , body :: Text
  , tags :: [Text]
  } deriving Show

postForm :: Maybe PostF -> Form PostF
postForm postf extra = do
  (titleRes, titleView) <- makeField textField title ["input-xlarge"]
  (bodyRes, bodyView) <- makeField textareaField (Textarea . body) ["span9"]
  (tagRes, tagView) <- makeField tagListField tags ["span3"]
  let postRes = PostF <$> titleRes <*> unTextarea `fmap` bodyRes <*> tagRes
  return (postRes, toWidget $(widgetFile "new-post"))
  where
    makeField :: Field Milagos Milagos a -> (PostF -> a) -> [Text]
                 -> MForm Milagos Milagos (FormResult a, FieldView Milagos Milagos)
    makeField field getter classes = mreq field settings (getter <$> postf)
      where settings = "" {
            fsAttrs = [("class", T.intercalate " " classes)]
       }


getNewPostR :: Handler RepHtml
getNewPostR = postR Nothing

postNewPostR :: Handler RepHtml
postNewPostR = do
  result <- fst . fst <$> runFormPost (postForm Nothing)
  case result of
    FormSuccess postf -> do
      _ <- runDB $ do
        postId <- insert $ mkPost postf
        -- get the keys and insert into the PostTag many-many table
        tagKeys <- loadTagKeys $ tags postf
        mapM (insert . PostTag postId) tagKeys
      redirect RootR
    _ -> redirect NewPostR

getEditPostR :: PostId -> Handler RepHtml
getEditPostR = postR . Just

postEditPostR :: PostId -> Handler RepHtml
postEditPostR postId = do
  newPost <- fst . fst <$> runFormPost (postForm Nothing)
  case newPost of
    FormSuccess postf -> do
      _ <- runDB $ do
        replace postId . mkPost $ postf
        -- out with the old tags, in with the new ones
        tagKeys <- loadTagKeys $ tags postf
        deleteWhere [PostTagPostId ==. postId]
        mapM (insert . PostTag postId) tagKeys

      redirect $ PostR postId
    _ -> redirect $ EditPostR postId

postR :: Maybe PostId -> Handler RepHtml
postR maybePostId = do
  postF <- maybe (return Nothing) (fmap Just . runDB . mkPostF) maybePostId
  (widget, enctype) <- generateFormPost . postForm $ postF
  let action = maybe NewPostR EditPostR maybePostId
  adminLayout [whamlet|
<div .row>
  <div .span12>
    <form action=@{action} method=post enctype=#{enctype}>
      ^{widget}
|]

-- Utility funnctions
-----------------------------------------------------


-- A field for a list of tags, which are comma-separated.
tagListField :: Field sub master [Text]
tagListField = Field {
    fieldParse = return . Right . Just . splitTagString
  , fieldView = \idAttr nameAttr attrs result _ -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} *{attrs} type=text value=#{either (const "") (T.intercalate ", ") result}>
|] }
  where splitTagString vals = filter (not . T.null) . map T.strip $ vals >>= T.splitOn ","

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

mkPost :: PostF -> Post
mkPost (PostF {title, body}) = Post title body
