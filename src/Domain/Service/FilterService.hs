module Domain.Service.FilterService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 
import qualified Domain.ImportEntity as E

class CommonService m  => FilterService m  where
    filterOfData        :: Day -> Either Error [m] -- API новостей  фильтрация по дате
    filterAuthor        :: Int -> Either Error [m]-- API новостей  фильтрация по имени автора
    filterCategory      :: Int -> Either Error [m]-- API новостей  фильтрация по категории по айди
    filterTeg           :: Int -> Either Error [m]-- API новостей  фильтрация по тега по айди
    -- filterName          :: Int -> Either Error [m] -- API новостей  фильтрация по название (вхождение подстроки)
    -- filterContent       :: [News] -> Either Error [m]-- API новостей  фильтрация по название контент (вхождение подстроки)
