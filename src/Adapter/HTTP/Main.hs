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

main :: ( MonadIO m, KatipContext m, AuthRepo m
        , EmailVerificationNotif m, SessionRepo m)
     => Int -> (m Response -> IO Response) -> IO ()
main port runner = do
  web <- Web.main runner
  api <- API.main runner
  run port $ vhost [(pathBeginsWith "api", api)] web
  where
    pathBeginsWith path req = headMay (pathInfo req) == Just path