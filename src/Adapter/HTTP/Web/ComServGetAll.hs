module Adapter.HTTP.Web.ComServGetAll where


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


routesAll :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m, CommonService m)
          => ScottyT e m ()
routesAll = do
       
        get "/all/:param" $ do 
            print "all"
            p   :: Text   <-  param "param" 
            result <- lift $ getAll p
            case result of
                Left er -> text "Error!!"
                Right res -> print res

            -- view' <- DF.getForm "authAdmin" formTest
            -- renderHtml $ createTestPage view' []
        
      
            -- (view', mayUser') <- runForm "authAdmin" formTest