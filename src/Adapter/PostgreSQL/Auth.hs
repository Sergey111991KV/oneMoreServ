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
import Adapter.PostgreSQL.CommonPostgres

findUsers:: PG r m
            => E.Login -> E.Password -> m (Maybe E.Users)  
findUsers login pass = do
    let rawLogin = E.rawLogin login
        rawPassw = E.rawPassword pass
    result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw)
    return $ case result of
        [auth] -> Just auth
        _ -> Nothing
    where
        qry = "SELECT * FROM user_blog WHERE login = (?) and password = (?)"


findUserIdByUser :: PG r m
               => E.Users -> m (Maybe E.UserId)           
findUserIdByUser (E.Users _ _ _ login pass _ _ author admin) = do
  let rawLogin = E.rawLogin login
      rawPassw = E.rawPassword pass
  result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw )
  return $ case result of
    [uId] -> Just uId
    _ -> Nothing
  where
    qry = "select id_user from user_blog where login = (?) and password = (?)"



newSession :: PG r m
               => E.UserId -> m E.SessionId          
newSession idU = do
    sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
    let newId = 4 :: Int
    result <- withConn $ \conn -> query conn qry (newId, sId, idU )
    case result of
        [sId] -> return sId
        err -> throwString $ "Unexpected error: " <> show err
    where
    qry = "INSERT INTO session (id, key ,user_id) values (?, ?,?)"



findUserIdBySessionId :: PG r m
              => SessionId -> m (Maybe UserId)
findUserIdBySessionId sId = do
    result <- withConn $ \conn -> query conn qry (sId)
    return $ case result of
        [uIdStr] -> Just uIdStr
        _        -> Nothing
    where
      qry = "select id, is_email_verified \
          \from auths \
          \where email = ? and pass = crypt(?, pass)"



