module Adapter.HTTP.Web.Auth where


import ClassyPrelude
import Web.Scotty.Trans
import Text.Digestive.Scotty
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import Text.Digestive.Form ((.:))
import Katip
import Text.Blaze.Html5 ((!))
import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Types.Status

import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S

data Auth = Auth Login Password

routes :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m)
          => ScottyT e m ()
routes = do
        get "/" $
            redirect "/users"

                -- home
        get "/users" $ do
            
            userId <- reqCurrentUserId
            print "Im here"
            mayAdmin <- lift $ findAccessAdminByUserId userId
            print "why not"
            case mayAdmin of
                Nothing -> do
                    raise $ stringError "Should not happen: user not found"
                    print "afas"
                Just isAdmin -> do 
                    renderHtml $ usersAdmin $ do
                          case isAdmin of 
                                AccessAdmin True -> "Is Admin"
                                AccessAdmin False -> "You are not Admin!!"
                -- register
        -- get "/auth/register" undefined
        -- post "/auth/register" undefined
        --         -- verify email
        -- get "/auth/verifyEmail/:code" undefined
        --         -- login
        get "/auth/login" $ do
            view <- DF.getForm "auth" authForm
            renderHtml $ loginPage view []
        post "/auth/login" $ do
            (view, mayAuth) <- runForm "auth" authForm
            case mayAuth of
                Nothing -> renderHtml $ loginPage view []
                Just (Auth log pass) -> do
                    result <- lift $ login log pass
                    case result of
                        Left _ -> renderHtml $ loginPage view ["Email/password is incorrect"]
                        Right sId -> do
                            setSessionIdInCookie sId
                            redirect "/"
        --         -- get user
        -- get "/users" undefined



usersAdmin :: Text -> H.Html
usersAdmin email =
  mainLayout "Admin" $ do
    H.div $
      H.h1 "Admin"
    H.div $
      H.toHtml email

loginPage :: DF.View [Text] -> [Text] -> H.Html
loginPage view msgs =
  mainLayout "Login" $ do
    H.div $
      authFormLayout view "Login" "/auth/login" msgs
    H.div $
      H.a ! A.href "/auth/register" $ "Register"

authFormLayout :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
authFormLayout view formTitle action msgs =
  formLayout view action $ do
    H.h2 $
      H.toHtml formTitle
    H.div $
      errorList msgs
    H.div $ do
      H.label "Email"
      DH.inputText "login" view
      H.div $
        errorList' "login"
    H.div $ do
      H.label "Password"
      DH.inputPassword "password" view
      H.div $
        errorList' "password"
    H.input ! A.type_ "submit" ! A.value "Submit"
    where
            errorList' path =
              errorList . mconcat $ DF.errors path view
            errorList =
              H.ul . concatMap errorItem
            errorItem =
              H.li . H.toHtml
-- logForm :: (Monad m) => DF.Form [Text] m Login
-- logForm =
--   "login" .: loginForm
--   where
--     loginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)
   
-- passForm :: (Monad m) => DF.Form [Text] m Password
-- passForm =
--   "password" .: passwordForm
--   where
--     passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)


authForm :: (Monad m) => DF.Form [Text] m Auth
authForm =
    Auth <$> "login" .: loginForm
        <*> "password" .: passwordForm
  where
        loginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)
        passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)