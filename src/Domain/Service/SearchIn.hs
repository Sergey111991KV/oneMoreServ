module Domain.Service.SearchIn where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 
import qualified Domain.ImportEntity as E

class CommonService m  => SearchIn m  where
    inContent :: Text -> m (Either E.Error [E.News] )   -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
    inEntyty  :: Text -> m (Either E.Error [E.News] )    --  либо в имени автора, либо в названии категории/тега

    -- nestedEntities :: Either Error [m] -- API c вложеныvb все сущности 
    -- filterOfData :: Day -> Either Error [m] -- API новостей  фильтрация по дате
    -- filterAuthor :: Author -> Either Error [m]-- API новостей  фильтрация по имени автора
    -- filterCategory :: Category -> Either Error [m]-- API новостей  фильтрация по категории по айди
    -- filterTeg :: Teg -> Either Error [m]-- API новостей  фильтрация по тега по айди
    -- filterName :: Text -> Either Error [m] -- API новостей  фильтрация по название (вхождение подстроки)
    -- filterContent :: [News] -> Either Error [m]-- API новостей  фильтрация по название контент (вхождение подстроки)
