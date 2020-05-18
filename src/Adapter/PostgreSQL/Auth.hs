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
--     -- fromField f mdata =
--     --     return login
--     --     where 
--     --         login = case login of
--     --             Just text -> Login   
--     --             Nothing      -> errSQLType

instance FromField E.Password where
    fromField field mb_bytestring = E.Password <$> fromField field mb_bytestring
    -- fromField field mb_bytestring = makeProductType mb_bytestring
    --     where
    --         makeProductType :: Maybe ByteString -> Conversion E.Password
    --         makeProductType (Just a ) = return fromRight undefined (mkPassword . decodeUtf8 a)
    --         makeTenantStatus Nothing = returnError UnexpectedNull field "Empty product type"
    -- fromField f mdata =
    --     if isJust mdata then 
    --         case fromJust mdata of
    --             Nothing -> returnError UnexpectedNull f ""
    --             a  -> fromRight undefined (mkPassword . decodeUtf8 a)
           
    --     else
            -- returnError UnexpectedNull f ""
        -- return pass
        -- where pass = case mdata of
        --     Just x -> mkLogin . decodeUtf8 x
        --     Nothing -> returnError UnexpectedNull f ""
                  
    


instance FromRow E.UserId where
    fromRow = E.UserId <$> field

instance FromRow E.Auth where
    fromRow = E.Auth <$> field <*> field <*> field <*> field

            ---  реализация авторизации

-- findAuth                :: Login -> Password -> m (Maybe Auth) 

findAuth:: PG r m
            => E.Login -> E.Password -> m (Maybe E.Auth)  
findAuth login pass = do
    let rawLogin = E.rawLogin login
        rawPassw = E.rawPassword pass
    result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw)
    return $ case result of
        [auth] -> Just auth
        _ -> Nothing
    where
        qry = "select id, is_email_verified \
          \from auths \
          \where email = ? and pass = crypt(?, pass)"


-- findUserIdByAuth      :: Auth -> m (Maybe UserId)

findUserIdByAuth :: PG r m
               => E.Auth -> m (Maybe E.UserId)           
findUserIdByAuth (E.Auth login pass author admin) = do
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




--     newSession            :: UserId -> m SessionId

-- newSession :: PG r m
--                => E.UserId -> m E.SessionId          
-- newSession uId = do
--         result <- withConn $ \conn -> query conn qry 



--     findUserIdBySessionId   :: SessionId -> m (Maybe UserId)

