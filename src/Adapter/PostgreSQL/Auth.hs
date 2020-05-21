module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Data.Pool
-- import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
-- import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField
import Data.Time
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Data.Has
import Text.StringRandom
import Data.Maybe
import Data.Either

import Domain.ImportEntity as E


type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

type State = Pool Connection

data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout :: NominalDiffTime
  }

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg action =
        bracket initPool cleanPool action
        where
          initPool = createPool openConn closeConn
                      (configStripeCount cfg)
                      (configIdleConnTimeout cfg)
                      (configMaxOpenConnPerStripe cfg)
          cleanPool = destroyAllResources
          openConn = connectPostgreSQL (configUrl cfg)
          closeConn = close

withState  ::  Config  -> ( State  ->  IO  a ) ->  IO  a
withState cfg action =
    withPool cfg $ \state -> do
        -- migrate state  --- можно добавлять дополнительные действия не меняя интерфейс главного действия withPool
        action state


withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn



instance FromField E.Login where
    fromField field mb_bytestring = E.Login <$> fromField field mb_bytestring

instance FromField E.Password where
    fromField field mb_bytestring = E.Password <$> fromField field mb_bytestring
                  

instance FromRow E.UserId where
    fromRow = E.UserId <$> field

instance FromRow E.User where
    fromRow = E.User <$> field <*> field <*> field <*> field

            ---  реализация авторизации

-- findAuth                :: Login -> Password -> m (Maybe Auth) 

findUser:: PG r m
            => E.Login -> E.Password -> m (Maybe E.User)  
findUser login pass = do
    let rawLogin = E.rawLogin login
        rawPassw = E.rawPassword pass
    result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw)
    return $ case result of
        [auth] -> Just auth
        _ -> Nothing
    where
        qry = "SELECT id FROM user WHERE login = login and password = pass"


-- findUserIdByAuth      :: Auth -> m (Maybe UserId)

findUserIdByAuth :: PG r m
               => E.User -> m (Maybe E.UserId)           
findUserIdByAuth (E.User login pass author admin) = do
  let rawLogin = E.rawLogin login
      rawPassw = E.rawPassword pass
  result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw, author, admin )
  return $ case result of
    [uId] -> Just uId
    _ -> Nothing
  where
    qry = "select id, is_email_verified \
          \from auths \
          \where email = ? and pass = crypt(?, pass)"


-- testConnect :: 

--     newSession            :: UserId -> m SessionId

-- newSession :: PG r m
--                => E.UserId -> m E.SessionId          
-- newSession uId = do
--         result <- withConn $ \conn -> query conn qry 



--     findUserIdBySessionId   :: SessionId -> m (Maybe UserId)

