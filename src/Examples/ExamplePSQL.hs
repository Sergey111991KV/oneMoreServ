module Examples.ExamplePSQL where

        

import ClassyPrelude
-- import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
-- import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField
import Data.Time
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Data.Has
import Data.Pool
import Data.Text.Time
import qualified Data.Text as Text
import qualified Data.ByteString as B

import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E


-- getAll :: Text -> IO ()
-- getAll x  
--                 -- | x == "author" = do
--                 --     return (Left E.AccessError)
--                 -- | x == "users"  = return (Left E.AccessError)
--                 -- | x == "tegs"   = return (Left E.AccessError)
--                 | x == "news"   = do
--                     result <- getAllNews
--                     let newResult = fmap convertNewsToEntity result 
--                     print newResult
--                     -- new <- fmap convertNewsToEntity result
                    
-- getAllNewsText :: Text  
-- getAllNewsText =  "news"

-- -- getAllNesw :: IO ([S.Entity])
-- -- getAllNesw = undefined

-- getAllNews :: IO [E.News]
-- getAllNews = do
--         let q = "SELECT * FROM news"
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         i    <- (query_ conn q  :: IO [E.News])
--         return i
        

-- convertNewsToEntity ::   E.News ->  S.Entity
-- convertNewsToEntity (E.News q w e r t y u i) =  S.EntNews (E.News q w e r t y u i)


-- type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

-- type State = Pool Connection

-- data Config = Config
--   { configUrl :: ByteString
--   , configStripeCount :: Int
--   , configMaxOpenConnPerStripe :: Int
--   , configIdleConnTimeout :: NominalDiffTime
--   }

-- withPool :: Config -> (State -> IO a) -> IO a
-- withPool cfg action =
--         bracket initPool cleanPool action
--         where
--           initPool = createPool openConn closeConn
--                       (configStripeCount cfg)
--                       (configIdleConnTimeout cfg)
--                       (configMaxOpenConnPerStripe cfg)
--           cleanPool = destroyAllResources
--           openConn = connectPostgreSQL (configUrl cfg)
--           closeConn = close

-- withState  ::  Config  -> ( State  ->  IO  a ) ->  IO  a
-- withState cfg action =
--     withPool cfg $ \state -> do
--         -- migrate state  --- можно добавлять дополнительные действия не меняя интерфейс главного действия withPool
--         action state


-- withConn :: PG r m => (Connection -> IO a) -> m a
-- withConn action = do
--   pool <- asks getter
--   liftIO . withResource pool $ \conn -> action conn

-- initState :: Config
-- initState = Config {
--                       configUrl = "localhost"
--                     , configStripeCount = 2
--                     , configMaxOpenConnPerStripe = 5
--                     , configIdleConnTimeout = 10
--                     }


-- findUsers:: PG r m
--             => E.Login -> E.Password -> m (Maybe E.Users)  
-- findUsers login pass = do
--     let rawLogin = E.rawLogin login
--         rawPassw = E.rawPassword pass
--     result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw)
--     return $ case result of
--         [auth] -> Just auth
--         _ -> Nothing
--     where
--         qry = "SELECT id FROM user WHERE login = login and password = pass"


-- findUserIdByUser :: PG r m
--                => E.Users -> m (Maybe E.UserId)           
-- findUserIdByUser (E.Users _ _ _ login pass _ _ author admin) = do
--   let rawLogin = E.rawLogin login
--       rawPassw = E.rawPassword pass
--   result <- withConn $ \conn -> query conn qry (rawLogin, rawPassw )
--   return $ case result of
--     [uId] -> Just uId
--     _ -> Nothing
--   where
--     qry = "select id, is_email_verified \
--           \from auths \
--           \where email = ? and pass = crypt(?, pass)"



-- newSession :: PG r m
--                => E.UserId -> m E.SessionId          
-- newSession idU = do
--     sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
--     result <- withConn $ \conn -> query conn qry (idU, sId )
--     case result of
--         [sId] -> return sId
--         err -> throwString $ "Unexpected error: " <> show err
--     where
--     qry = "select id, is_email_verified \
--           \from auths \
--           \where email = ? and pass = crypt(?, pass)"



-- findUserIdBySessionId :: PG r m
--               => SessionId -> m (Maybe UserId)
-- findUserIdBySessionId sId = do
--     result <- withConn $ \conn -> query conn qry (sId)
--     return $ case result of
--         [uIdStr] -> Just uIdStr
--         _        -> Nothing
--     where
--       qry = "select id, is_email_verified \
--           \from auths \
--           \where email = ? and pass = crypt(?, pass)"

-- create  :: PG r m => Bool -> (Maybe S.Entity) -> m (Either E.Error S.Entity )
-- -- create = undefined
-- create False _      = return (Left E.AccessError)
-- create True Nothing = return (Left E.DataError)
-- create True (Just (S.EntAuthor (E.Author idA text idU)))  = do
--         result <- withConn $ \conn -> query conn qry (idA, text, idU)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idA, text, idU)]         ->  Right (S.EntAuthor (E.Author idA text idU)) 
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
-- create True (Just (S.EntCategory (S.CatCategory1 (E.Category1 idCF text))))  = do
--         result <- withConn $ \conn -> query conn qry (idCF, text)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
    


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

ss :: IO String
ss = do
        let t = 2 :: Int
        let s = 2 :: Int
        conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
        [Only i]  <- query conn "select description from author where user_id = (?) and id = (?)" (s ,t)
        print i
        return i



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



--                                  new
-- create ::  May
create  (E.EntUsers users) = do
        conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
        result <-  execute conn q users
        return $ case result of
            _        ->  Left E.DataError
            i        ->  Right i
        where
            q = "insert into user_blog (id_user, name, last_name, login, password, avatar, data_create, admini, author) values (?,?,?,?,?,?,?,?,?)"
            -- "INSERT INTO author (id, description ,user_id) values (?,?,?)"
            

testUser  ::  E.Entity                                             
testUser  = E.EntUsers 
                    (E.Users uid name lastname login password avatar dataCr admin author)
            where
                uid = E.UserId 5
                name = "Igor"
                lastname = "Popov"
                login = E.Login "victor@test.com"
                password = E.Password "1234ABCDefgh"
                avatar = "url//"
                dataCr = parseISODateTime ("2011-11-19 18:28:52.607875 UTC")
                admin =  E.AccessAdmin False
                author = False

-- create testUser


    -- :l Examples.ExamplePSQL