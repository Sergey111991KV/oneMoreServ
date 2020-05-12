module Domain.Service.CommonService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time


data Error = AccessError | DataError

class CommonService a where
    create  :: Maybe m -> Either Error m
    editing :: m -> Either Error m
    getAll  :: Either Error [m]
    getOne  :: Int -> Either Error  m
    remove  :: m -> Either Error ()
    

class  CategoryService a where
    nestedCategory  :: a -> [b]
    allTreeCategory :: a -> [a]
    
instance CategoryService Category1 where
    nestedCategory a =  undefined
   

instance CategoryService Category2 where
    nestedCategory a = undefined
    
instance CategoryService Category3 where
    nestedCategory a = undefined
    
    

-- instance CommonService Category1 where
   
-- instance CommonService Category2 where
  
-- instance CommonService Category3 where

instance CommonService Category where

instance CommonService Author where
-- create m    = undefined
-- editing m   =  undefined
-- getAll      = undefined
-- getOne  i   = undefined
-- remove  m   = undefined
               
instance CommonService Comment where

instance CommonService Draft where

instance CommonService News where

instance CommonService Teg where

instance CommonService User where
    

data Category = forall a. CategoryService a =>  Category a
data Entity   = forall a. CommonService a =>  Entity a

gg :: [Entity]
gg = [Entity (Author 44 44 "daf"), Entity (Category (Category1 22 "adf"))] -- пример


class CommonService a => FilterService a where
    nestedEntities :: Either Error [m] -- API c вложеныvb все сущности 
    filterOfData :: Day -> Either Error [m] -- API новостей  фильтрация по дате
    filterAuthor :: Author -> Either Error [m]-- API новостей  фильтрация по имени автора
    filterCategory :: Category -> Either Error [m]-- API новостей  фильтрация по категории по айди
    filterTeg :: Teg -> Either Error [m]-- API новостей  фильтрация по тега по айди
    filterName :: Text -> Either Error [m] -- API новостей  фильтрация по название (вхождение подстроки)
    filterContent :: [News] -> Either Error [m]-- API новостей  фильтрация по название контент (вхождение подстроки)




class CommonService a => SearchIn a where
    inContent :: a -> [a]   -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
    inEntyty  :: a -> [a]   --  либо в имени автора, либо в названии категории/тега


class (FilterService a, CommonService a) => SortedOf a where        --     API новостей должно поддерживать сортировку по:
    sortedDate      :: Day -> [a]           -- дате,
    sortedAuthor    :: Author -> [a]        -- автору (имя по алфавиту),
    sortedCategory  :: [a] -> [a] -- по категориям (название по алфавиту), 
    sortedPhoto     :: [a] -> [a] -- по количеству фотографий







-- data Entyties = forall a. CommonService a =>  Entyties a

-- data family EntityFamily a