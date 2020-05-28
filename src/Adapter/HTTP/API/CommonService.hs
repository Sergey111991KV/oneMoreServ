module Adapter.HTTP.API.CommonService where

import ClassyPrelude
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Network.HTTP.Types.Status
import Data.Aeson ()
import Katip

import Adapter.HTTP.Common

import Domain.ImportEntity as E
import Domain.ImportService


--     create  :: Maybe Entity  -> m (Either E.Error Entity )
--     editing :: Entity -> m (Either E.Error Entity)
--     getAll  :: String -> m (Either E.Error [Entity])
--     getOne  :: String -> Int -> m (Either E.Error  Entity)
--     remove  :: Entity -> m (Either E.Error ())

-- routes :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m)
--           => ScottyT e m ()
-- routes = do
--         post "/api/create/login" $ do
--                 inputLog  <- parseAndValidateJSON logForm
--                 inputPass <- parseAndValidateJSON passForm
--                 domainResult <- lift $ login inputLog inputPass
--                 case domainResult of
--                   Left LoginError -> do
--                     status status400
--                     json ("InvalidAuth" :: Text)
--                   Right sId -> do
--                     setSessionIdInCookie sId
--                     return ()



-- logForm :: (Monad m) => DF.Form [Text] m Login
-- logForm =
--   "login" .: loginForm
--   where
--     loginForm = DF.validate (toResult . mkLogin) (DF.text Nothing)