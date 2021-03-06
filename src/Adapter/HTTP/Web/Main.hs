module Adapter.HTTP.Web.Main where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Gzip

import qualified Adapter.HTTP.Web.Auth as Auth
import qualified Adapter.HTTP.Web.Menu as Menu
import Adapter.HTTP.Web.CommonService.CommonService as Common
import Adapter.HTTP.Web.SearchIn.SearchIn  as SearchIn
import Adapter.HTTP.Web.FilterService.FilterService as Filter

import Domain.ImportEntity as E
import Domain.ImportService as S

mainWEB :: ( MonadIO m, SessionRepo m, CommonService m, SearchIn m,  FilterService m)
     => (m Response -> IO Response) -> IO Application
mainWEB runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT runner $ routes cacheContainer

routes :: ( MonadIO m,  SessionRepo m, CommonService m, SearchIn m,  FilterService m)
       => CacheContainer -> ScottyT LText m ()
routes cacheContainer = do
  middleware $
    gzip $ def { gzipFiles = GzipCompress }
--   middleware $
--     staticPolicyWithOptions cacheContainer (addBase "src/Adapter/HTTP/Web/static")
  Menu.routesMenu
  Auth.routesAuth
  Common.routesCommon
  SearchIn.routesSearchInNewsText
  Filter.routesFilter


  notFound $ do
    status status404
    text "Not found"

  -- defaultHandler $ \e -> do
  --   lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
  --   status status500
  --   text "Internal server error!"