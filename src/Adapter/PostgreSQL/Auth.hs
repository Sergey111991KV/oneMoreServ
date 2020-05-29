module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Data.Pool
-- import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
import Database.PostgreSQL.Simple.ToField 
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
    deleteOldSession idU
    insertNewSession idU
    result <- withConn $ \conn -> query conn qry (idU )
    case result of
        [sId] -> do
            print $ sessionRaw sId
            return sId
        err -> throwString $ "Unexpected error: " <> show err
    where
    qry = "select key from session where user_id= ?"


deleteOldSession :: PG r m
                => E.UserId  -> m Int64 
deleteOldSession idU  = do
        result <- withConn $ \conn -> execute conn qry ( idU )
        return result 
        where
            qry = "delete from session where user_id = ?"

insertNewSession :: PG r m
                => E.UserId  -> m Int64 
insertNewSession  idU  = do
    sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
    result <- withConn $ \conn -> execute conn qry ( sId, idU )
    return result 
    where
        qry = "INSERT INTO session (key ,user_id) values (?,?)"     

findUserIdBySessionId :: PG r m
              => E.SessionId -> m (Maybe E.UserId)
findUserIdBySessionId sId = do
    result <- withConn $ \conn -> query conn qry (sId)
    return $ case result of
        [uIdStr] -> Just uIdStr
        _        -> Nothing
    where
      qry = "select user_id from session where key = ? "



findAccessAdminByUserId :: PG r m
                => E.UserId -> m (Maybe Bool)
findAccessAdminByUserId uId = do
    result <- withConn $ \conn -> query conn qry (uId)
    return $ case result of
        [access] -> Just access
        _        -> Nothing
    where
      qry = "select admin from user_blog where id_user = ? "



-- findAccessAuthorByUserId :: PG r m
--                 => UserId -> m (Maybe Bool)
-- findAccessAuthorByUserId uId = do
--     result <- withConn $ \conn -> query conn qry (uId)
--     return $ case result of
--         [bool] -> Just bool
--         _        -> Nothing
--     where
--       qry = "select id, is_email_verified \
--           \from auths \
--           \where email = ? and pass = crypt(?, pass)"




-- -- instance FromRow Int where 
-- --     fromRow = fromRow
-- -- -- instance ToRow Bool



-- instance FromField Password where
-- -- instance  ToField Password
-- instance FromJSON Password
-- instance ToJSON Password
-- instance  ToRow Password