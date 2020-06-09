

module Adapter.PostgreSQL.CommonService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except
import Katip
import Database.PostgreSQL.Simple
import Data.Maybe
import Data.Text.Time


import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E
import Adapter.PostgreSQL.CommonPostgres as CP
import Adapter.PostgreSQL.CommonService.ImportCommon as CSP



    
create  :: PG r m =>  S.Entity -> m (Either E.Error Int64 )
-- create = undefined
-- create  (Just (S.EntAuthor (E.Author idA text idU)))  = do
--         result <- withConn $ \conn -> query conn qry (idA, text, idU)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idA, text, idU)]         ->  Right (S.EntAuthor (E.Author idA text idU)) 
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
create   (S.EntUsers users)  = do
        print "createUsers"
        result <- withConn $ \conn -> execute conn q users
        return $ case result of
            _        ->  Left E.DataError
            i        ->  Right i
        where
            q = "insert into user_blog (id_user, name, last_name, login, password, avatar, data_create, admini, author) values (?,?,?,?,?,?,?,?,?)"
        
create   (S.EntNews news)  = do
        print "createNews"
        result <- withConn $ \conn -> execute conn q news
        return $ case result of
            _        ->  Left E.DataError
            i        ->  Right i
        where
            q = "insert into user_blog (id_user, name, last_name, login, password, avatar, data_create, admini, author) values (?,?,?,?,?,?,?,?,?)"
          

editing :: PG r m =>  S.Entity -> m (Either E.Error Int64 )
editing (S.EntUsers users) = do
        print "editUsers"
        result <- withConn $ \conn -> execute conn q (E.name users, E.lastName users, E.authLogin users, E.authPassword users, E.avatar users, E.dataCreate users, E.authAdmin users, E.authAuthor users, E.id_user users)
        return $ case result of
            _        ->  Left E.DataError
            i        ->  Right i
        where
            uId = E.rawUserId $ E.id_user users
            q = "UPDATE user_blog SET name = (?), last_name = (?), login = (?), password = (?), avatar = (?), data_create = (?), admini = (?), author = (?) where id = (?);"




-- create  (Just (S.EntCategory (S.CatCategory1 (E.Category1 idCF text))))  = do
--         result <- withConn $ \conn -> query conn qry (idCF, text)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
                              
                              
-- create  (Just (S.EntCategory (S.CatCategory2 (E.Category2 idCF text refId))))  = do
--         result <- withConn $ \conn -> query conn qry (idCF, text)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
                              
-- create  (Just (S.EntCategory (S.CatCategory3 (E.Category3 idCF text refId))))  = do
--         result <- withConn $ \conn -> query conn qry (idCF, text)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"




-- EntAuthor   E.Author   | 
-- EntCategory Category   | 
-- EntComment  E.Comment  | 
-- EntDraft    E.Draft    |
-- EntNews     E.News     | 
-- EntUsers    E.Users    | 
-- EntTeg      E.Teg
-- create True (Just (S.Entity a))  = do
--     return $ Right (S.Entity a)
-- create True (Just (S.Entity (x)))  = do
    
--     return $ Right (S.Entity(E.Author 22 "324" 33 ))
-- create True (Just (S.Entity(x)))  = do
--     case x of 
--             E.Author 22 "324" 33 -> return $ Right (S.Entity(E.Author 22 "324" 33 ))
    -- return $ Right (S.Entity(E.Author 22 "324" 33 ))
-- create True (Just (E.Author x y z)) = do
--     return $ Right (S.Entity $ (E.Author x y z))

                    


           

-- editing :: PG r m => Int -> m (Either E.Error S.Entity)
-- editing False _ = return (Left E.AccessError)
-- editing True (S.EntAuthor (E.Author idA text idU)) = do
--         result <- withConn $ \conn -> query conn qry (idA, text, idU)
--         return $ case result of
--             _                          ->  Left E.DataError
--             [(idA, text, idU)]         ->  Right (S.EntAuthor (E.Author idA text idU)) 
--         where
--                         qry = "insert into auths \
--                               \(email, pass, email_verification_code, is_email_verified) \
--                               \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"





getAll :: PG r m => Text -> m (Either E.Error [S.Entity])
getAll x  
                | x == "author" = undefined
                | x == "users"  = undefined
                | x == "tegs"   = undefined
                | x == "news"   = do
                    print "allNews"
                    let q = "SELECT * FROM news"
                    result <- (withConn $ \conn -> query_ conn q  :: IO [E.News])
                    let  newResult = fmap convertNewsToEntity result
                    print newResult
                    return $ case newResult of
                            [ ]             ->  Left E.DataError
                            newResult        ->  Right newResult
          
                
        

convertNewsToEntity ::   E.News ->  S.Entity
convertNewsToEntity (E.News q w e r t y u i) =  S.EntNews (E.News q w e r t y u i)


getOne :: PG r m => Int -> Text ->  m (Either E.Error  S.Entity)
getOne  idE text   
                    | text == "news" = do
                            let q = "SELECT * FROM news where id = (?)"
                            i <- (withConn $ \conn -> query conn q [idE] :: IO [E.News])
                            return $ case i of
                                    [x]     -> Right (convertNewsToEntity x)
                                    _      -> Left E.DataError
                            
                            -- let newResult = convertNewsToEntity i

                            -- case newResult of
                            --     result -> return $ Right newResult
                            --     _      -> return $ Left E.DataError

-- getOneTest :: Int -> IO (S.Entity)
-- getOneTest idE  = do
--         let q = "SELECT * FROM news where id = (?)"
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'"
--         [i] <- (query conn q [idE] :: IO [E.News])
--         return (convertNewsToEntity i)
        -- let newResult = convertNewsToEntity i
        -- return  newResult 




-- ss :: IO String
-- ss = do
--         let t = 2 :: Int
--         let s = 2 :: Int
--         conn <- connectPostgreSQL "host='localhost' port=5431 dbname='hblog'" 
--         [Only i]  <- query conn "select description from author where user_id = (?) and id = (?)" (s ,t)
--         print i
--         return i


-- remove :: PG r m => Bool ->  S.Entity -> m (Either E.Error ())
-- remove False _  = return (Left E.AccessError)
-- remove True (S.EntAuthor (E.Author idA text idU)) = return (Left E.AccessError)


-- create  :: Maybe Entity  -> m (Either E.Error Entity )
--     editing :: Int -> m (Either E.Error Entity)
--     getAll  :: m (Either E.Error [Entity])
--     getOne  :: Int -> m (Either E.Error  Entity)
--     remove  :: Int -> m (Either E.Error ())


-- class  CategoryService a where
--     allNestedCategory  :: a -> [a]
--     allUpCategory      :: a -> [a]
--     allCategory        :: a -> [a]




--                          Examples


-- create' :: PG r m =>  E.Users -> m (Either E.Error Int64 )
-- create'   users  = do
--         print "createUsers"
--         result <- withConn $ \conn -> execute conn q users
--         return $ case result of
--             _        ->  Left E.DataError
--             i        ->  Right i
--         where
--             q = "insert into user_blog (id_user, name, last_name, login, password, avatar, data_create, admini, author) values (?,?,?,?,?,?,?,?,?)"
                     
