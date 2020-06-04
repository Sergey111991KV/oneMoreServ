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
import Data.Text.Time

import Domain.Validation.Validation
import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S


routesCommon :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m, CommonService m)
          => ScottyT e m ()
routesCommon = do
        get "/iduser" $ do
            (UserId uid) <- reqCurrentUserId
            let result = show uid
            -- view <- DF.getForm "idUser" idForm
            renderHtml $ idPage (pack result)
        get "/create/user" $ do 
            view <- DF.getForm "user" userForm
            (UserId newId) <- lift $ newUserId
            renderHtml $ createUserPage newId view []
        
        post "/create/user" $ do 
            (view, mayUser) <- runForm "user" userForm
            (UserId newId) <- lift $ newUserId
            case mayUser of
                Nothing ->
                    renderHtml $ createUserPage newId view ["Data is incorrect"]
                Just user -> do
                    result <- lift $ create ( Just (EntUsers user))
                    case result of
                        Left LoginErrorEmailNotVerified -> text "Email has not been verified"
                        Right sId -> do
                            text "ned to trans base of data"
        
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
mkId  = parseTextId' . decimal 

parseTextId' :: Either String (Integer, Text) -> Either [ErrMsg] UserId
parseTextId' (Left err) = Left [pack err]
parseTextId' (Right (uId, txt)) = Right (UserId (fromIntegral uId))

mkName :: Text -> Either [ErrMsg] Text
mkName txt = 
    case t of
        ""  -> Left [err]
        _   ->  Right txt
    where 
        t   = unpack txt
        err = "Error parse Name" :: Text


mkLastName :: Text -> Either [ErrMsg] Text
mkLastName txt = 
        case t of
            ""  -> Left [err]
            _   ->  Right txt
        where 
            t   = unpack txt
            err = "Error parse LastName" :: Text

mkAvatar :: Text -> Either [ErrMsg] Text
mkAvatar txt = 
        case t of
            ""  -> Left [err]
            _   ->  Right txt
        where 
            t   = unpack txt
            err = "Error parse URL Avatar" :: Text

mkdataCreate :: Text -> Either [ErrMsg] UTCTime
mkdataCreate txt = do
    let  result = parseUTCTimeOrError txt
    case result of
        Left err -> Left [pack err]
        Right time -> Right time


authAuthorCreate :: Bool -> Either [ErrMsg] Bool
authAuthorCreate bool = Right bool
   

authAdminCreate :: Bool -> Either [ErrMsg] AccessAdmin
authAdminCreate bool = Right (AccessAdmin bool)


createUserPage :: Int -> DF.View [Text] -> [Text] -> H.Html
createUserPage uId view msgs = 
  mainLayout "CreateUser" $ do
    H.div $
        createFormLayout uId view "CreateUser" "/create/user" msgs
    H.div $
      H.a ! A.href "/create" $ "Back"






-- [pack txt] -- тот момент когда функция возвращает то что надо по типу, но не так как задумывалась))))

createFormLayout :: Int -> DF.View [Text] -> Text -> Text -> [Text] -> H.Html
createFormLayout i view formTitle action msgs =
  formLayout view action $ do
    H.h2 $
      H.toHtml formTitle
    H.div $
      errorList msgs
    H.div $ do
      H.label newId
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
    H.div $ do
      H.label "Data Create"
      DH.inputPassword "dataCreate" view
      H.div $
        errorList' "dataCreate"
    H.div $ do
      H.label "Author"
      DH.inputPassword "authAuthor" view
      H.div $
        errorList' "authAuthor"
    H.div $ do
      H.label "Admin"
      DH.inputPassword "authAdmin" view
      H.div $
        errorList' "authAdmin"
    H.input ! A.type_ "submit" ! A.value "Submit"
  where
    errorList' path =
      errorList . mconcat $ DF.errors path view 
    errorList =
      H.ul . concatMap errorItem
    errorItem =
      H.li . H.toHtml
    newId = H.toHtml $ show i



idPage :: Text -> H.Html
idPage msg = 
    mainLayout " Проверка id "  $  do
        H.h1 " Проверка id "
        H.div  $  H.toHtml msg



idForm :: (Monad m) => DF.Form [Text] m UserId
idForm = undefined
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