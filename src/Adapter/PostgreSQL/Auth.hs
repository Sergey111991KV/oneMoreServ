module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Data.Pool
-- import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Data.Time

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

withState cfg action =
    withPool cfg $ \state -> do
        -- migrate state  --- можно добавлять дополнительные действия не меняя интерфейс главного действия withPool
        action state