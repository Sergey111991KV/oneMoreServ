module Domain.Service.FilterService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 
import qualified Domain.ImportEntity as E

class CommonService m  => FilterService m  where
    filterOfData        :: Text -> m (Either E.Error [E.News]) -- API новостей  фильтрация по дате
    filterAuthor        :: Int -> m (Either E.Error [E.News])-- API новостей  фильтрация по имени автора
    filterCategory      :: Int -> m (Either E.Error [E.News])-- API новостей  фильтрация по категории по айди
    filterTeg           :: Int -> m (Either E.Error [E.News])-- API новостей  фильтрация по тега по айди
    -- filterName          :: Int -> Either Error [m] -- API новостей  фильтрация по название (вхождение подстроки)
    -- filterContent       :: [News] -> Either Error [m]-- API новостей  фильтрация по название контент (вхождение подстроки)

    -- не понял последние два пункта и нужно было конечно сделать фильтрацию не по инт а по скажем IdAuthor - но может потом реализую))
