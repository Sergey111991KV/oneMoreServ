module Adapter.HTTP.API where

import ClassyPrelude
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Network.HTTP.Types.Status
import Data.Aeson ()
import Katip

routes :: ( ScottyError e, MonadIO m)
          => ScottyT e m ()
routes = do
    get "/api/news" $ text "news"
    -- тут дохера всего
    get "/api/authors" $ text "authors" 
    -- — и создание, и редактирование, и получение, и удаление, только для админов, 
    get "/api/category" $ text "category" 
    -- получение всем, создание, удаление и редактирование только для админов, 
    get "/api/teg" $ text "teg" 
    --  получение всем, создание, удаление и редактирование только для админов, 
    get "/api/draft" $ text "draft" 
    --создание, редактирование, получение, удаление всем авторам только своих черновиков,
    -- плюс отдельный метод publish, чтобы апдейтнуть публикацию
    get "/api/user" $ text "user" 
    -- пользователей  — создание, получение всем (редактирования нет), удаление только админам
    get "/api/comments" $ text "comments" 
    -- комментарии — создание, получение списка комментариев для определенного поста , удаление. 
    --Редактирование и получение отдельного комментария необязательны.