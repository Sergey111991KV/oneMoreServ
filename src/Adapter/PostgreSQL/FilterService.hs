module Adapter.PostgreSQL.FilterService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except
import Katip
import Database.PostgreSQL.Simple
import Data.Maybe
import Data.Text.Time
import Data.Attoparsec.Text
import Data.Text
import Data.Time.Calendar

import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E
import Adapter.PostgreSQL.CommonPostgres as CP

-- filterOfData'     :: PG r m => Day -> m (Either E.Error [E.News]) -- API новостей  фильтрация по дате
-- filterOfData' day = do
--     let q = "SELECT * FROM news where data_create >= (?)"
--     result <- (withConn $ \conn -> query conn q [day'] :: IO [E.News])
--     return $ case result of
--         [ ]             ->  Left E.DataError
--         result          ->  Right result
--     where
--             day' = showGregorian day


            -- Text <=> Day

filterOfData :: PG r m => Text -> m (Either E.Error [E.News]) 
filterOfData day = do
    let q = "SELECT * FROM news where data_create >= (?)"
    result <- withConn $ \conn -> query conn q [day] :: IO [E.News]
    return $ case result of
            [ ]             ->  Left E.DataError
            result          ->  Right result
    


filterAuthor :: PG r m => Int -> m (Either E.Error [E.News])-- API новостей  фильтрация по имени автора
filterAuthor idA = do
    let q = "SELECT * FROM news where authors_id = (?)"
    result <- withConn $ \conn -> query conn q [idA] :: IO [E.News]
    return $ case result of
        [ ]             ->  Left E.DataError
        result          ->  Right result





-- filterCategory      :: Int -> m (Either Error [E.News])-- API новостей  фильтрация по категории по айди
-- filterTeg           :: Int -> m (Either Error [E.News])-- API новостей  фильтрация по тега по айди