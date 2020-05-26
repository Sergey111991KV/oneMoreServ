module Adapter.HTTP.Main where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
-- import qualified Adapter.HTTP.API.Auth as AuthAPI
import Adapter.HTTP.Common
import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip
import Domain.ImportEntity as E
import Domain.ImportService

main :: ( MonadIO m, KatipContext m, SessionRepo m)
     => Int -> (m Response -> IO Response) -> IO ()
main port runner =
  scottyT port runner routes

routes :: ( MonadIO m, KatipContext m, SessionRepo m)
          => ScottyT LText m ()
routes = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }

--   AuthAPI.routes

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)