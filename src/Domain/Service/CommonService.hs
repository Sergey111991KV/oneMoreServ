module Domain.Service.CommonService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time

data Error = AccessError | DataError

class CommonService a where
    create  :: Either Error m
    editing :: m -> Either Error m
    getAll  :: Either Error [m]
    getOne  :: Either Error  m
    remove  :: m -> Either Error ()

class CommonService a =>  ExtendedService a where
    nestedEntities :: Either Error [m] -- API c вложеныvb все сущности 
    filterOfData :: Day -> Either Error [nestedEntities] -- API новостей  фильтрация по дате
    -- API новостей  фильтрация по имени автора
    -- API новостей  фильтрация по категории по айди
    -- API новостей  фильтрация по тега по айди
    -- API новостей  фильтрация по название (вхождение подстроки)
    -- API новостей  фильтрация по название контент (вхождение подстроки)




    -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
    --  либо в имени автора, либо в названии категории/тега


--     API новостей должно поддерживать сортировку по:
-- дате,
-- автору (имя по алфавиту), 
-- по категориям (название по алфавиту), 
-- по количеству фотографий





-- data Entyties = forall a. CommonService a =>  Entyties a

-- data family EntityFamily a