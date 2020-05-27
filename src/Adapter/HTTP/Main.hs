module Adapter.HTTP.Main where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Adapter.HTTP.Common
import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip
import Domain.ImportEntity as E
import Domain.ImportService
import qualified Adapter.HTTP.API.Auth as AuthAPI

mainHTTP :: ( MonadIO m, KatipContext m, SessionRepo m)
     => Int -> (m Response -> IO Response) -> IO ()
mainHTTP port runner =
  scottyT port runner routes

routes :: ( MonadIO m, KatipContext m, SessionRepo m)
          => ScottyT LText m ()
routes = do

  AuthAPI.routes

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)