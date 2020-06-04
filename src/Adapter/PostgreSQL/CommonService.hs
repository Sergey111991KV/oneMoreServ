module Adapter.PostgreSQL.CommonService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except
import Katip
import Database.PostgreSQL.Simple
import Data.Maybe


import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E
import Adapter.PostgreSQL.CommonPostgres as CP
import Adapter.PostgreSQL.CommonService.ImportCommon as CSP



    
create  :: PG r m => (Maybe S.Entity) -> m (Either E.Error S.Entity )
-- create = undefined
create  _      = return (Left E.AccessError)
create  Nothing = return (Left E.DataError)
create  (Just (S.EntAuthor (E.Author idA text idU)))  = do
        result <- withConn $ \conn -> query conn qry (idA, text, idU)
        return $ case result of
            _                          ->  Left E.DataError
            [(idA, text, idU)]         ->  Right (S.EntAuthor (E.Author idA text idU)) 
        where
                        qry = "insert into auths \
                              \(email, pass, email_verification_code, is_email_verified) \
                              \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
create  (Just (S.EntCategory (S.CatCategory1 (E.Category1 idCF text))))  = do
        result <- withConn $ \conn -> query conn qry (idCF, text)
        return $ case result of
            _                          ->  Left E.DataError
            [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
        where
                        qry = "insert into auths \
                              \(email, pass, email_verification_code, is_email_verified) \
                              \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
                              
                              
create  (Just (S.EntCategory (S.CatCategory2 (E.Category2 idCF text refId))))  = do
        result <- withConn $ \conn -> query conn qry (idCF, text)
        return $ case result of
            _                          ->  Left E.DataError
            [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
        where
                        qry = "insert into auths \
                              \(email, pass, email_verification_code, is_email_verified) \
                              \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"
                              
create  (Just (S.EntCategory (S.CatCategory3 (E.Category3 idCF text refId))))  = do
        result <- withConn $ \conn -> query conn qry (idCF, text)
        return $ case result of
            _                          ->  Left E.DataError
            [(idCF, text)]             ->  Right (S.EntCategory ( S.CatCategory1 (E.Category1 idCF text)))
        where
                        qry = "insert into auths \
                              \(email, pass, email_verification_code, is_email_verified) \
                              \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"




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
    
-- getAll :: PG r m => Bool -> String -> m (Either E.Error [S.Entity])
-- getAll False _ = return (Left E.AccessError)
-- getAll True x  
--                 | x == "author" = return (Left E.AccessError)
--                 | x == "users"  = return (Left E.AccessError)
--                 | x == "tegs"   = return (Left E.AccessError)

-- getOne :: PG r m => Bool -> String ->  Int -> m (Either E.Error  S.Entity)
-- getOne False _ _   = return (Left E.AccessError)
-- getOne True  t idE = return (Left E.AccessError)

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