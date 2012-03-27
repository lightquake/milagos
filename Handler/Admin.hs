module Handler.Admin where

import Import

postForm :: Form Post
postForm = renderDivs $ Post
           <$> areq textField "Title" Nothing
           <*> areq textareaField "Body" Nothing


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
    FormSuccess post -> do
      runDB $ insert post
      redirect RootR