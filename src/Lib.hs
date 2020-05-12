module Lib
    ( someFunc
    ) where

import Adapter.HTTP.Main 
import ClassyPrelude
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Database.PostgreSQL.Simple
import Adapter.PostgreSQL.APIConnection
import Data.Pool

makeDbConfig :: C.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  return $ DbConfig <$> name
                    <*> user
                    <*> password



someFunc :: IO ()
someFunc = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
      Nothing -> putStrLn "No database configuration found, terminating..."
      Just conf -> do
        pool <- createPool (newConn conf) close 1 40 10
        putStrLn "database configuration found!!!"  
        mainH 3000 id
