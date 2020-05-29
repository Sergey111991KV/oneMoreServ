module Adapter.HTTP.Web.Common where

import ClassyPrelude
import Web.Scotty.Trans
import qualified Text.Digestive.View as DF
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as H


import Adapter.HTTP.Common

import Domain.ImportEntity as E
import Domain.ImportService as S
