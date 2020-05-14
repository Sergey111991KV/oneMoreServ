module Adapter.HTTP.Common where

import ClassyPrelude
import Web.Scotty.Trans
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Aeson as DF
import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Domain.ImportEntity

type MyServer s = ScottyT Error (IO)
type MyAction s = ActionT Error (IO)

parseAndValidateJSON :: (ScottyError e, MonadIO m, ToJSON v)
                     => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) ->
      return result
-- В общем функция анализирует Форму (проверка входных данных) и если все норм -> возвращает порт соединения


-- parseAndValidateJSON :: (ScottyError e, MonadIO m, ToJSON v)
--                       => DF.Form v m a -> ActionT e m a
-- parseAndValidateJSON form = do
--     val <- jsonData 'rescue' (\_ -> return Null)
--     validationResult <- lift $ DF.digestJSON form val
--     case validationResult of
--         (v, Nothing) -> do
--             status status400 -- статус 
--             json $ DF.jsonErrors v -- получение ошибки 
--             finish
--         (_, Just result) ->
--             return result 
--   я не знаю какого хрена - но эта функция не работает!!!!!

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie = setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie
-- преобразовать печеньку)) в порт

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val