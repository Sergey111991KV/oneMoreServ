module Adapter.HTTP.API.APIAuth where

import ClassyPrelude
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Network.HTTP.Types.Status
import Data.Aeson ()
import Katip
import Domain.ImportEntity
import Domain.Service.ServiceAuth.ServiceAuth
import Domain.Service.CommonService





import Adapter.HTTP.Common




routesAuth :: ( ScottyError e, MonadIO m)
          => Access -> ScottyT e m ()
routesAuth acc = do
  post "/api/auth/login" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
      Left LoginErrorInvalidAuth -> do
        status status400
        json ("InvalidAuth" :: Text)
      Left LoginErrorEmailNotVerified -> do
        status status400
        json ("EmailNotVerified" :: Text)
      Right sId -> do
        setSessionIdInCookie sId
        return ()

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm =
          Auth <$> "login" .: loginForm
               <*> "password" .: passwordForm
          where
            loginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)
            passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)