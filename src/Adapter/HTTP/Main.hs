module Adapter.HTTP.Main where

import ClassyPrelude hiding (delete)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API as API
import Katip
import Network.Wai
import Domain.ImportEntity
import Adapter.PostgreSQL.APIConnection

-- middleware $ basicAuth (verifyCredentials pool)
--                            "Haskell Blog Realm" { authIsProtected = protectedResources }

-- authApp :: State -> Either Error Users
-- authApp 


mainH :: ( MonadIO m) => Int -> (m Response -> IO Response) -> IO ()
mainH port runner =
  scottyT port runner routes

routes :: ( MonadIO m) => ScottyT LText m ()
routes = do
  API.routes