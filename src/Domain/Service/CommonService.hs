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

class CommonService a =>  CategoryService a where
    nestedCategory  :: a -> [b]
    allTreeCategory :: a -> [a]

instance CategoryService Category1 where
    nestedCategory a =  undefined

instance CategoryService Category2 where
    nestedCategory a = undefined

instance CategoryService Category3 where
    nestedCategory a = undefined

instance CommonService Category1 where

instance CommonService Category2 where

instance CommonService Category3 where
    

data Category = forall a. CategoryService a =>  Category a


lst :: [Category]
lst = [Category (Category1 10 "adf"), Category (Category2 50 "Sadf" 150)]



class CommonService a =>  FilterService a where
    nestedEntities :: Either Error [m] -- API c вложеныvb все сущности 
    filterOfData :: Day -> Either Error [m] -- API новостей  фильтрация по дате
    filterAuthor :: Author -> Either Error [m]-- API новостей  фильтрация по имени автора
    filterCategory :: Category -> Either Error [m]-- API новостей  фильтрация по категории по айди
    filterTeg :: Teg -> Either Error [m]-- API новостей  фильтрация по тега по айди
    filterName :: Text -> Either Error [m] -- API новостей  фильтрация по название (вхождение подстроки)
    filterContent :: [News] -> Either Error [m]-- API новостей  фильтрация по название контент (вхождение подстроки)




    -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
    --  либо в имени автора, либо в названии категории/тега


--     API новостей должно поддерживать сортировку по:
-- дате,
-- автору (имя по алфавиту), 
-- по категориям (название по алфавиту), 
-- по количеству фотографий





-- data Entyties = forall a. CommonService a =>  Entyties a

-- data family EntityFamily a