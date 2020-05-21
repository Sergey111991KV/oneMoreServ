module Examples.ExamplePSQL where

        
import Katip
import ClassyPrelude
-- import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
-- import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField
import Domain.ImportEntity as E
import Data.Time
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Data.Has
import Data.Pool
import qualified Data.Text as Text
import qualified Data.ByteString as B





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

initState :: Config
initState = Config {
                      configUrl = "postgresql://localhost/hauth"
                    , configStripeCount = 2
                    , configMaxOpenConnPerStripe = 5
                    , configIdleConnTimeout = 10
                    }

-- instance FromField E.Login where
--                             fromField field mb_bytestring = E.Login <$> fromField field mb_bytestring
                        
-- instance FromField E.Password where
--                             fromField field mb_bytestring = E.Password <$> fromField field mb_bytestring
                                          
                        
-- instance FromRow E.UserId where
--                             fromRow = E.UserId <$> field
                        
-- instance FromRow E.Users where
--                             fromRow = E.Users <$> field <*> field <*> field <*> field <*> field <*> field <*> field  <*> field <*> field

                    -- Подключение

    -- conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'"
-- hello :: IO Int
-- hello = do
--   conn <- connectPostgreSQL ""
--   [Only i] <- query_ conn "select 2 + 2"
--   return i

-- myConf :: ConnectInfo
-- myConf = ConnectInfo {
--     connectHost = "localhost"	 
--     , connectPort = 5431
--     , connectUser = ""	
--     , connectPassword = "" 	
--     , connectDatabase = "hblog"	 

--         }

-- dd :: IO Int
-- dd = do
--     let q = "select authors_id from news where id = ?" 
--     conn <- connect myConf
--     [Only i] <- query conn q (1 :: Integer)
--     return i

-- dd' :: IO Int
-- dd'= do
--     conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'"
--     [Only i] <- query_ conn "select id from author where user_id = 2"
--     return i




                                    -- получить

                                    -- 1 параметр
-- dd' :: IO Int
-- dd'= do
--     let t = 2
--     conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'"
--     [Only i]  <- query conn "select id from author where user_id = (?)" ([2] :: [Int])
--     return i

-- dd'' :: IO Int
-- dd'' = do
--     let t = [2] :: [Int]
--     conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'"
--     [Only i]  <- query conn "select id from author where user_id = (?)" t
--     return i

-- --     -- Как оказалось возвращается массив объектов - поэтому были проблемы

-- dd :: IO Int
-- dd = do
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'"
--         [Only i] <- query_ conn "select id from author where user_id = 2"
--         return i
    


                                    -- 2 параметра
-- instance ToRow Bool where

-- dd :: IO String
-- dd = do
--         let t = 2 :: Int
--         let s = 2 :: Int
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         [Only i]  <- returning conn "select description from author where user_id = (?) and id = (?)" [(s ,t)]
--         return i   ---??????

-- ss :: IO String
-- ss = do
--         let t = 2 :: Int
--         let s = 2 :: Int
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         [Only i]  <- query conn "select description from author where user_id = (?) and id = (?)" (s ,t)
--         print i
--         return i



                                    -- сущность


-- dd :: IO ()
-- dd = do
--         let s = [2] :: [Int]
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         mapM_ print =<< (query conn q  s :: IO [E.Author])
--             where q = "SELECT * FROM author where user_id = (?)"
        
-- dd' :: IO [E.Author]
-- dd' = do
--         let s = [2] :: [Int]
--         let q = "SELECT * FROM author where user_id = (?)"
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         i  <- (query conn q  s :: IO [E.Author])
--         return i

-- dd'' :: IO [E.Author]
-- dd''  = do
--         let q = "SELECT * FROM author"
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         i  <- (query_ conn q  :: IO [E.Author])
--         B.putStrLn i
--         return i


                             
                                    -- отправить
-- dd'' :: IO Int64
-- dd''  = do
--         let q = "INSERT INTO author (id, description ,user_id) values (?,?,?)"
--         let s = (3 :: Int, "dfsd" :: String, 1 :: Int)
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         i <- execute conn q s 
--         return i
--      здесь отправляю по частям
        
       
-- dd' :: IO Int64
-- dd'  = do
--                 let q = "INSERT INTO author (id, description ,user_id) values (?,?,?)"
--                 let s = Author 4 "dfsd" 1
--                 conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--                 i <- execute conn q s 
--                 return i
-- по сущностям

                                    -- модифицировать
-- dd :: IO Int64
-- dd  = do
--                 let q = "UPDATE author SET id = (?), description = (?) ,user_id = (?) where id = 3;"
--                 let s = Author 5 "dfsdhjhfssd" 1
--                 conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--                 i <- execute conn q s 
--                 return i

                                    -- удалить

-- dd :: IO Int64
-- dd  = do
--                 let q = "DELETE FROM author WHERE id = (?) and description = (?) and user_id = (?);"
--                 let s = Author 5 "dfsdhjhfssd" 1
--                 conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--                 i <- execute conn q s 
--                 return i

    -- :l Examples.ExamplePSQL