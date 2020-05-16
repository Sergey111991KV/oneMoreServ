module Adapter.HTTP.API.APINews where

import ClassyPrelude
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Network.HTTP.Types.Status
import Data.Aeson ()
import Katip
import Domain.Service.CommonService
import Domain.ImportEntity


routesNews :: ( ScottyError e, MonadIO m)
          => Access -> ScottyT e m ()
routesNews acc = do
    get "/api/news" $ text "news"
    -- тут дохера всего