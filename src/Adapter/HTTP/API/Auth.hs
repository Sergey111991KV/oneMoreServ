module Adapter.HTTP.API.Auth where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.ImportEntity
import Domain.ImportService
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Network.HTTP.Types.Status
import Data.Aeson ()
import Katip


                    -- AUTH FORM

logForm :: (Monad m) => DF.Form [Text] m Login
logForm =
  "login" .: loginForm
  where
    loginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)
   
passForm :: (Monad m) => DF.Form [Text] m Password
passForm =
  "password" .: passwordForm
  where
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)


routes :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m)
          => ScottyT e m ()
routes = do

                    --  FROM FORM

        post "/api/auth/login" $ do
                inputLog  <- parseAndValidateJSON logForm
                inputPass <- parseAndValidateJSON passForm
                domainResult <- lift $ login inputLog inputPass
                case domainResult of
                  Left LoginError -> do
                    status status400
                    json ("InvalidAuth" :: Text)
                  Right sId -> do
                    setSessionIdInCookie sId
                    return ()

                    -- PARAM
        
        post "/api/login/:login/password/:password" $ do
                logins   :: Text   <-      param "login" 
                password :: Text   <-       param "password" 
                let log = mkLogin logins
                -- "victor@test.com"
                let pas = mkPassword password
                -- "1234ABCDefgh"
                case log of 
                    Left _ -> do
                        status status400
                        json ("wrong login" :: Text)
                    Right l -> do
                        case pas of 
                            Left _ -> do
                                status status400
                                json ("wrong passw" :: Text)
                            Right p -> do
                                domainResult <- lift $ login  l p
                                -- fromStrict password ++ fromStrict login
                                case domainResult of
                                    Left LoginError -> do
                                        status status400
                                        json ("InvalidAuth" :: Text)
                                    Right sId -> do
                                        setSessionIdInCookie sId
                                        status status200
                                        return ()
                                       
                        -- EXAMPLE ACCESS AUTHORS AND ADMIN

        get "/api/admin/:userId" $ do
                uId   :: Int   <-      param "userId" 
                let idUser = UserId uId
                access <- lift $ findAccessAdminByUserId idUser
                case access of
                        Just acc -> do
                            case acc of 
                                True -> text "Доступ Админа"
                                False -> text "Нет Доступа Админа"
                        Nothing -> text "Пользователь с таким id не обнаружен"

                        
                
                                 
        -- post "/api/author/:userId" $ do
        --         uId   :: Int   <-      param "userId" 
        --         let idUser = UserId uId
        --         access <- lift $ findAccessAuthorByUserId idUser
        --         case access of
        --             Just acc -> do
        --                 case acc of 
        --                     True -> text "adf"
        --                     False -> text "adf"
        --             Nothing -> text "adf"
             
                                        