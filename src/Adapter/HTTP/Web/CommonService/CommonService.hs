module Adapter.HTTP.Web.CommonService.CommonService where



import ClassyPrelude
import Web.Scotty.Trans
import Text.Digestive.Scotty
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import Text.Digestive.Form ((.:))
import Text.Blaze.Html5 ((!))
import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Types.Status
import Data.Text.Read
import Data.Text.Time
import System.IO.Unsafe

import Domain.Validation.Validation
import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S

import Adapter.HTTP.Web.CommonService.ComServCreate as Create
import Adapter.HTTP.Web.CommonService.ComServGetAll as All
import Adapter.HTTP.Web.CommonService.ComServGetOne as One
import Adapter.HTTP.Web.CommonService.ComServRemove as Remove


routesCommon :: ( ScottyError e, MonadIO m, SessionRepo m, CommonService m)
          => ScottyT e m ()
routesCommon = do
        Create.routesComCreate
        All.routesAll
        One.routesOne
        Remove.routesRemove