module Adapter.HTTP.Web.FilterService.FilterService where



import ClassyPrelude
import Web.Scotty.Trans
import Text.Digestive.Scotty
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.View as DF
import Text.Digestive.Form ((.:))
import Katip
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


routesFilter :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m, CommonService m, FilterService m)
          => ScottyT e m ()
routesFilter = do
        get "/filter/data/:param" $ do 
            print "filterData"
            p   :: Text   <-  param "param" 
            result <- lift $ filterOfData p
            case result of
                Left er -> text "Not right data"
                Right res -> print res

        get "/filter/author/:param" $ do 
            print "filterAuthor"
            p   :: Int   <-  param "param" 
            result <- lift $ filterAuthor p
            case result of
                Left er -> text "Not author id"
                Right res -> print res
