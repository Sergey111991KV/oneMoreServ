module Adapter.HTTP.Web.Menu where


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

import Adapter.HTTP.Common
import Adapter.HTTP.Web.Common

import Domain.ImportEntity as E
import Domain.ImportService as S


routesMenu :: ( ScottyError e, MonadIO m, KatipContext m, SessionRepo m)
          => ScottyT e m ()
routesMenu = do
        get "/menu" $
            renderHtml $ menuPage "Menu"





menuPage :: Text -> H.Html
menuPage msg = do
        mainLayout "Users" $ do
            H.div $
                H.h1 "Users"
            H.div $
                H.toHtml msg
            H.div $
                H.a ! A.href "/menu/authors" $ "Authors"
            H.div $
                H.a ! A.href "/menu/news" $ "News"
            H.div $
                H.a ! A.href "/menu/category" $ "Category"
            H.div $
                H.a ! A.href "/menu/tags" $ "Tags"
            H.div $
                H.a ! A.href "/menu/administration" $ "Administration"
            H.div $
                H.a ! A.href "/menu/cabinet" $ "Users Cabinet"