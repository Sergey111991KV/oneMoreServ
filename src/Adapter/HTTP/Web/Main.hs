module Adapter.HTTP.Web.Main where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Katip
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Gzip

import qualified Adapter.HTTP.Web.Auth as Auth

import Domain.ImportEntity as E
import Domain.ImportService as S