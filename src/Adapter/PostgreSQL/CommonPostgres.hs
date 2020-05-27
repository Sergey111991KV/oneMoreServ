module Adapter.PostgreSQL.CommonPostgres where

import ClassyPrelude
import Data.Pool
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow 
import Database.PostgreSQL.Simple
import Data.Time
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Data.Has
import Text.StringRandom
import Data.Maybe
import Data.Either

import Domain.ImportEntity as E
import Domain.ImportService

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
        -- migrate state  можно добавлять дополнительные действия не меняя интерфейс главного действия withPool
        action state

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn