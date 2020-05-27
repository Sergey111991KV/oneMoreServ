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
