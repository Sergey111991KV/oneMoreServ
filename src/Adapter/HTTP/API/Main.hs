module Adapter.HTTP.API.Main where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Adapter.HTTP.Common
import Network.Wai
import Network.Wai.Middleware.Gzip

import Domain.ImportEntity  as E
import Domain.ImportService as S

import Adapter.HTTP.API.Common
import qualified Adapter.HTTP.API.Auth as AuthAPI

mainAPI :: ( MonadIO m, SessionRepo m)
     => (m Response -> IO Response) -> IO Application
mainAPI runner =
        scottyAppT runner routes
      

routes :: ( MonadIO m, SessionRepo m)
          => ScottyT LText m ()
routes  = undefined       
-- routes = do

--   AuthAPI.routes
  
--   defaultHandler $ \e -> do
--     lift $  ErrorS $ "Unhandled error: " <>  (showError e)
--     status status500
--     json ("InternalServerError" :: Text)