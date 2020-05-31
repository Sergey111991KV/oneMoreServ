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

import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S


-- routes :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m)
--           => ScottyT e m ()
-- routes = do

--         get "/create/user" $ do 
--             view <- DF.getForm "user" userForm
--             renderHtml $ createUserPage view []
        
--         -- post "/create/user" $ do 
--         --     (view, mayUser) <- runForm "auth" userForm
--         --     case mayUser of
--         --         Nothing ->
--         --             renderHtml $ userPage view ["Data is incorrect"]
--         --         Just user -> do
--         --             result <- lift $ createUser user
--         --             case result of
--         --                 Left LoginErrorEmailNotVerified -> renderHtml $ loginPage view ["Email has not been verified"]
--                         -- Right sId -> do
        
userForm :: (Monad m) => DF.Form [Text] m Users
userForm = do
    result <- lift $ newUserId
    case result of
        Nothing          -> (DF.text Nothing)
        Just  nUid       -> do
            Users   <$> nUid
                    <*> "name"      .: nameForm
                    <*> "lastName"  .: lastNameForm
                    <*> "authLogin"  .: authLoginForm
                    <*> "authPassword"  .: authPasswordForm
                    <*> "avatar"  .: avatarForm
                    <*> "dataCreate"  .: dataCreateForm
                    <*> "authAuthor"  .: authAuthorForm
                    <*> "authAdmin"  .: authAdminForm
  where
        nameForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
        lastNameForm
        authLoginForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
        authPasswordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)
        avatarForm
        dataCreateForm
        authAuthorForm
        authAdminForm


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