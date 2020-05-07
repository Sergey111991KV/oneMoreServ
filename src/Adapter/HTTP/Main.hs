module Adapter.HTTP.Main where

import ClassyPrelude hiding (delete)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API as API
import Katip
import Network.Wai

mainH :: ( MonadIO m) => Int -> (m Response -> IO Response) -> IO ()
mainH port runner =
  scottyT port runner routes

routes :: ( MonadIO m) => ScottyT LText m ()
routes = do
  -- middleware $ gzip $ def { gzipFiles = GzipCompress }
  API.routes