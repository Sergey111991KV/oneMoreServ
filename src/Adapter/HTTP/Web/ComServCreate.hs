module Adapter.HTTP.Web.ComServCreate where


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
import System.IO.Unsafe

import Domain.Validation.Validation
import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S


routesComCreate :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m, CommonService m)
          => ScottyT e m ()
routesComCreate = do
       
        get "/create/user" $ do 
            view <- DF.getForm "user" userForm
            renderHtml $ createUserPage view []

            -- view' <- DF.getForm "authAdmin" formTest
            -- renderHtml $ createTestPage view' []
        
        post "/create/user" $ do 
            print "post"
            (view, mayUser) <- runForm "user" userForm
            
            -- (view', mayUser') <- runForm "authAdmin" formTest
            
            print mayUser
            case mayUser of
                Nothing ->
                    renderHtml $ createUserPage view ["Data is incorrect"]
                Just user -> do
                    let newUser = S.EntUsers user
                    print newUser
                    result <- lift $ create  newUser
                    case result of
                        Left err -> text "don't to base"
                        Right sId -> do
                            text "all good"
        
userForm :: Monad m =>  DF.Form [Text] m (E.Users)
userForm = do
        -- EntUsers    E.Users  
            Users   <$> "id"        .: idForm --  проверил 
                    <*> "name"      .: nameForm -- проверил
                    <*> "lastName"  .: lastNameForm -- проверил
                    <*> "authLogin"  .: authLoginForm -- true
                    <*> "authPassword"  .: authPasswordForm --
                    <*> "avatar"  .: avatarForm -- 
                    <*> "dataCreate"  .: dataCreateForm -- succses
                    <*> "authAdmin"  .: authAdminForm
                    <*> "authAuthor"  .: authAuthorForm
  where
        idForm = DF.validate (toResult . mkId) (DF.text Nothing) 
        nameForm = DF.validate (toResult . mkName) (DF.text Nothing)
        lastNameForm = DF.validate (toResult . mkLastName) (DF.text Nothing)
        authLoginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)
        authPasswordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)
        avatarForm = DF.validate (toResult . mkAvatar) (DF.text Nothing)
        dataCreateForm = DF.validate (toResult . mkdataCreate) (DF.text Nothing)
        authAuthorForm = DF.validate (toResult . authAuthorCreate) (DF.text Nothing)
        authAdminForm = DF.validate (toResult . authAdminCreate) (DF.text Nothing)

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


authAuthorCreate :: Text -> Either [ErrMsg] Bool
authAuthorCreate txt = 
    case t of
        "False"   ->  Right False
        "True"    ->  Right True
        _         -> Left [err]
    where 
        t   = unpack txt
        err = "Error parse Bool Author! Put True or False" :: Text
   

authAdminCreate :: Text -> Either [ErrMsg] AccessAdmin
authAdminCreate txt =
    case t of
        "False"   ->  Right (AccessAdmin False)
        "True"    ->  Right (AccessAdmin True)
        _         -> Left [err]
    where 
        t   = unpack txt
        err = "Error parse Bool Admin! Put True or False" :: Text
   


createUserPage ::DF.View [Text] -> [Text] -> H.Html
createUserPage view msgs = 
  mainLayout "CreateUser" $ do
    H.div $
        createFormLayout view "CreateUser" "/create/user" msgs
    H.div $
      H.a ! A.href "/create" $ "Back"

createFormLayout :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
createFormLayout view formTitle action msgs =
  formLayout view action $ do
    H.h2 $
      H.toHtml formTitle
    H.div $
      errorList msgs
    H.div $ do
      H.label "   Put this Id"
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
      H.label  newTime
      H.label "   Put Data Create"
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
    newTime = H.toHtml $ formatISODateTime $ unsafePerformIO getCurrentTime



-- idPage :: Text -> H.Html
-- idPage msg = 
--     mainLayout " Проверка id "  $  do
--         H.h1 " Проверка id "
--         H.div  $  H.toHtml msg




-- formTest :: (Monad m) => DF.Form [Text] m AccessAdmin
-- formTest =
--     "authAdmin"  .: authAdminForm
--     where
--             authAdminForm = DF.validate (toResult . authAdminCreate) (DF.text Nothing)

-- createTestPage ::  DF.View [Text] -> [Text] -> H.Html
-- createTestPage view msgs = 
--   mainLayout "CreateUser" $ do
--     H.div $
--         createFormLayoutTest  view "CreateUser" "/create/user" msgs
    
-- createFormLayoutTest :: DF.View [Text] -> Text -> Text -> [Text] -> H.Html
-- createFormLayoutTest view formTitle action msgs =
--   formLayout view action $ do
--     H.h2 $
--       H.toHtml formTitle
--     H.div $
--       errorList msgs
--     H.div $ do
--       H.label "authAdmin"
--       DH.inputText "authAdmin" view
--       H.div $
--         errorList' "authAdmin"
--     H.input ! A.type_ "submit" ! A.value "Submit"
--     where
--         errorList' path =
--             errorList . mconcat $ DF.errors path view 
--         errorList =
--             H.ul . concatMap errorItem
--         errorItem =
--             H.li . H.toHtml
--         newTime = H.toHtml $ formatISODateTime $ unsafePerformIO getCurrentTime