module Adapter.HTTP.Main where

import ClassyPrelude

import Katip
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Vhost


import Domain.ImportEntity as E
import Domain.ImportService as S
import qualified Adapter.HTTP.API.Main as API
import qualified Adapter.HTTP.Web.Main as Web

mainALL :: ( MonadIO m, KatipContext m, SessionRepo m, CommonService m, SearchIn m)
     => Int -> (m Response -> IO Response) -> IO ()
mainALL port runner = do
  web <- Web.mainWEB runner
  api <- API.mainAPI runner
  run port $ vhost [(pathBeginsWith "api", api)] web
  where
    pathBeginsWith path req = headMay (pathInfo req) == Just path