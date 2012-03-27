{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.PasswordAuth
       ( authPassword
       ) where

import Prelude

import Crypto.BCrypt

import Data.Text (Text)
import Data.Text.Encoding as E
import Text.Hamlet (hamletFile, shamlet)

import Yesod.Auth
import Yesod.Dispatch
import Yesod.Form (runInputPost, textField, ireq)
import Yesod.Handler
import Yesod.Widget

-- required to make () a valid AuthId
instance PathPiece () where
  fromPathPiece = const $ Just ()
  toPathPiece = const "()"

authName :: Text
authName = "password"


postLoginR :: YesodAuth master => Text -> GHandler Auth master ()
postLoginR valid = do
  password <- runInputPost $ ireq textField "password"
  if validatePassword (E.encodeUtf8 valid) (E.encodeUtf8 password)
     then setCreds True $ Creds authName "()" []
     else do setMessage [shamlet|Invalid password.|]
             toMaster <- getRouteToMaster
             redirect $ toMaster LoginR

-- | Take a function from a route converter to a form and return an
-- AuthPlugin. The form must have a 'password' field containing the
-- password.
authPassword :: YesodAuth m => Text -> AuthPlugin m
authPassword valid = AuthPlugin authName dispatch $ form

  where dispatch "POST" ["login"] = postLoginR valid >>= sendResponse
        dispatch _ _              = notFound
        form :: (Route Auth -> Route m) -> GWidget sub m ()
        form th = addHamlet $(hamletFile "templates/login.hamlet")