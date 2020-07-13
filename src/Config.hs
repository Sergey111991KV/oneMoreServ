module Config where

import ClassyPrelude
import System.Environment
import qualified Adapter.PostgreSQL.CommonPostgres as PG

data  Config  =  Config
  {  configPort     ::  Int
  ,  configPG       ::  PG.Config
  }

devConfig = Config
  { configPort = 3000
  , configPG = PG.Config
    { PG.configUrl = " host='localhost' port=5431 dbname='hblog'"
    , PG.configStripeCount = 2
    , PG.configMaxOpenConnPerStripe = 5
    , PG.configIdleConnTimeout = 10
    }
  }