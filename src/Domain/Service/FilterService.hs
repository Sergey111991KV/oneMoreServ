module Domain.Service.FilterService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 

class CommonService m  => FilterService m  where
    -- nestedEntities :: Either Error [m] -- API c вложеныvb все сущности 
    -- filterOfData :: Day -> Either Error [m] -- API новостей  фильтрация по дате
    -- filterAuthor :: Author -> Either Error [m]-- API новостей  фильтрация по имени автора
    -- filterCategory :: Category -> Either Error [m]-- API новостей  фильтрация по категории по айди
    -- filterTeg :: Teg -> Either Error [m]-- API новостей  фильтрация по тега по айди
    -- filterName :: Text -> Either Error [m] -- API новостей  фильтрация по название (вхождение подстроки)
    -- filterContent :: [News] -> Either Error [m]-- API новостей  фильтрация по название контент (вхождение подстроки)
