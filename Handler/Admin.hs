module Handler.Admin where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Handler.Renderers
import           Import
import           Text.Blaze.Renderer.Text

data PostF = PostF {
    title :: Text
  , body :: Html
  , tags :: [Text]
  }

-- Need this because the actual htmlField has a comment in its hamlet.
htmlField' :: RenderMessage master FormMessage => Field sub master Html
htmlField' = htmlField { fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
<textarea id="#{theId}" name="#{name}" *{attrs}>#{showVal val}|]
                       }
               where showVal = either id (L.toStrict . renderHtml)

postForm :: Maybe PostF -> Form PostF
postForm postf extra = do
  (titleRes, titleView) <- makeField textField title ["input-xlarge"]
  (bodyRes, bodyView) <- makeField htmlField' body ["span12"]
  (tagRes, tagView) <- makeField tagListField tags ["span6"]
  let postRes = PostF <$> titleRes <*> bodyRes <*> tagRes
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
        post <- mkPost postf
        postId <- insert post
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
  -- if 'Delete' was clicked, delete the post
  isDelete <- runInputPost $ iopt boolField "delete"
  liftIO $ print isDelete
  when (isJust isDelete) $ case newPost of
    FormSuccess _ -> do
      -- have to do this manually since sqlite doesn't cascade
      runDB $ delete postId >> deleteWhere [PostTagPostId ==. postId]
      redirect RootR
    _ -> redirect $ EditPostR postId


  case newPost of
    FormSuccess postf -> do
      _ <- runDB $ do
        -- preserve the time posted so edits don't pop up top
        oldTime <- postPosted <$> get404 postId
        post <- mkPost postf
        replace postId $ post { postPosted = oldTime }
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

mkPost :: MonadIO m => PostF -> m Post
mkPost (PostF {title, body}) = liftIO (Post title body <$> getCurrentTime)