{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation
    ( Milagos (..)
    , Route (..)
    , MilagosMessage (..)
    , resourcesMilagos
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    ) where

import           Prelude

import           Control.Applicative ((<$>))

import qualified Settings
import           Yesod
import           Yesod.Auth
import           Yesod.Default.Config
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Logger (logMsg, formatLogText)
import           Yesod.Static

import           Database.Persist.GenericSql
import qualified Database.Persist.Store
import           Model
import           Settings (widgetFile, Extra (..))
import           Text.Blaze.Renderer.Text (renderHtml)
import           Text.Hamlet (hamletFile)
import           Text.Jasmine (minifym)
import           Web.ClientSession (getKey)

import           Data.Text (Text)
import           Model.PasswordAuth
import           Types

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.

-- Set up i18n messages. See the message folder.
mkMessage "Milagos" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype MilagosRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Milagos = MilagosRoute
-- * Creates the value resourcesMilagos which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Milagos. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the MilagosRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Milagos" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Milagos Milagos (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Milagos where
    approot = ApprootMaster $ appRoot . settings

    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        blogTitle <- extraTitle . appExtra . settings <$> getYesod
        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- /admin requires authorization
    isAuthorized NewPostR = needsAuth
    isAuthorized (EditPostR _) = needsAuth
    isAuthorized _ = const $ return Authorized


    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

needsAuth :: YesodAuth m => t -> GHandler s m AuthResult
needsAuth _ = maybe AuthenticationRequired (const Authorized) <$> maybeAuthId

-- How to run database actions.
instance YesodPersist Milagos where
    type YesodPersistBackend Milagos = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)



instance YesodAuth Milagos where
    type AuthId Milagos = ()

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId = const . return $ Just ()

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins app =
      [authPassword (extraPassword . appExtra . settings $ app)]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Milagos FormMessage where
    renderMessage _ _ = defaultFormMessage

isEmptyHtml :: Html -> Bool
isEmptyHtml = (== "") . renderHtml