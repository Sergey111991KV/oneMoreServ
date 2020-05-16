module Adapter.PostgreSQL.APIConnection where

import ClassyPrelude
import Database.PostgreSQL.Simple
import qualified Domain.Service.CommonService as CS
import Data.Has
import Control.Monad.Catch (MonadThrow)
import Data.Pool
import Data.Either
import Data.Maybe




type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)
type State = Pool Connection

data DbConfig = DbConfig {
     dbName :: String,
     dbUser :: String,
     dbPassword :: String
     }
     deriving (Show, Generic)


newConn :: DbConfig -> IO Connection
newConn conf = connect defaultConnectInfo
                            { connectUser = dbUser conf
                            , connectPassword = dbPassword conf
                            , connectDatabase = dbName conf
                            }
     
-- lastId :: 

-- -- conn <- connect defaultConnectInfo { connectDatabase = "haskell" }



