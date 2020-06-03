module Adapter.HTTP.Web.CommonService where


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
import Data.Text.Read

import Domain.Validation.Validation
import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S


routesCommon :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m)
          => ScottyT e m ()
routesCommon = do

        get "/create/user" $ do 
            view <- DF.getForm "user" userForm
            renderHtml $ createUserPage view []
        
        -- post "/create/user" $ do 
        --     (view, mayUser) <- runForm "user" userForm
        --     case mayUser of
        --         Nothing ->
        --             renderHtml $ userPage view ["Data is incorrect"]
        --         Just user -> do
        --             result <- lift $ createUser user
        --             case result of
        --                 Left LoginErrorEmailNotVerified -> renderHtml $ loginPage view ["Email has not been verified"]
        --                 Right sId -> do
        --                     text "ned to trans base of data"
        
userForm :: Monad m =>  DF.Form [Text] m Users
userForm = do
            Users   <$> "id"        .: idForm
                    <*> "name"      .: nameForm
                    <*> "lastName"  .: lastNameForm
                    <*> "authLogin"  .: authLoginForm
                    <*> "authPassword"  .: authPasswordForm
                    <*> "avatar"  .: avatarForm
                    <*> "dataCreate"  .: dataCreateForm
                    <*> "authAuthor"  .: authAuthorForm
                    <*> "authAdmin"  .: authAdminForm
  where
        idForm = DF.validate (toResult . mkId) (DF.text Nothing)
        nameForm = DF.validate (toResult . mkName) (DF.text Nothing)
        lastNameForm = DF.validate (toResult . mkLastName) (DF.text Nothing)
        authLoginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)
        authPasswordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)
        avatarForm = DF.validate (toResult . mkAvatar) (DF.text Nothing)
        dataCreateForm = DF.validate (toResult . mkdataCreate) (DF.text Nothing)
        authAuthorForm = DF.validate (toResult . authAuthorCreate) (DF.bool Nothing)
        authAdminForm = DF.validate (toResult . authAdminCreate) (DF.bool Nothing)

mkId :: Text -> Either [ErrMsg] UserId
mkId txt = undefined


--  do
--     result <- decimal txt
--     case result of
--         Left err -> Left err :: [ErrMsg]
--         Right (uId, t) -> Right uId

mkName :: Text -> Either [ErrMsg] String
mkName txt = undefined

mkLastName :: Text -> Either [ErrMsg] String
mkLastName txt = undefined

mkAvatar :: Text -> Either [ErrMsg] String
mkAvatar txt = undefined

mkdataCreate :: Text -> Either [ErrMsg] UTCTime
mkdataCreate txt = undefined


authAuthorCreate :: Bool -> Either [ErrMsg] Bool
authAuthorCreate txt = undefined


authAdminCreate :: Bool -> Either [ErrMsg] AccessAdmin
authAdminCreate txt = undefined


createUserPage :: DF.View [Text] -> [Text] -> H.Html
createUserPage view msgs = 
  mainLayout "CreateUser" $ do
    H.div $
        createFormLayout view "CreateUser" "/create/user" msgs
    H.div $
      H.a ! A.href "/create" $ "Back"

stringErrMsg :: String -> [ErrMsg]
stringErrMsg txt = pack txt

createFormLayout :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
createFormLayout view formTitle action msgs =
  formLayout view action $ do
    H.h2 $
      H.toHtml formTitle
    H.div $
      errorList msgs
    H.div $ do
      H.label "Id"
      DH.inputText "id" view
      H.div $
        errorList' "id"
    H.div $ do
      H.label "Name"
      DH.inputPassword "name" view
      H.div $
        errorList' "name"
    H.div $ do
      H.label "LastName"
      DH.inputText "lastName" view
      H.div $
        errorList' "lastName"
    H.div $ do
      H.label "Login"
      DH.inputPassword "authLogin" view
      H.div $
        errorList' "passauthLoginword"
    H.div $ do
      H.label "Password"
      DH.inputText "authPassword" view
      H.div $
        errorList' "authPassword"
    H.div $ do
      H.label "Avatar"
      DH.inputPassword "avatar" view
      H.div $
        errorList' "avatar"
    H.input ! A.type_ "submit" ! A.value "Submit"
  where
    errorList' path =
      errorList . mconcat $ DF.errors path view 
    errorList =
      H.ul . concatMap errorItem
    errorItem =
      H.li . H.toHtml

-- authForm :: (Monad m) => DF.Form [Text] m Auth
-- authForm =
--   Auth <$> "email" .: emailForm
--        <*> "password" .: passwordForm
--   where
--     emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
--     passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

--     data  Users = Users
--     { 
--       id_user      :: UserId
--     , name         :: String
--     , lastName     :: String
--     , authLogin    :: Login
--     , authPassword :: Password
--     , avatar       :: String
--     , dataCreate   :: UTCTime
--     , authAuthor   :: Bool
--     , authAdmin    :: AccessAdmin




--         -- create  :: Maybe Entity  -> m (Either E.Error Entity )
--         -- editing :: Int -> m (Either E.Error Entity)
--         -- getAll  :: m (Either E.Error [Entity])
--         -- getOne  :: Int -> m (Either E.Error  Entity)
--         -- remove  :: Int -> m (Either E.Error ())