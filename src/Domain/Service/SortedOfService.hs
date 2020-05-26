module Domain.Service.SortedOfService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 
import Domain.Service.FilterService 

class (FilterService m , CommonService m ) => SortedOf m  where        --     API новостей должно поддерживать сортировку по:
    sortedDate      :: Day -> m [a]           -- дате,
    sortedAuthor    :: Author -> m [a]        -- автору (имя по алфавиту),
    sortedCategory  :: [a] -> m [a] -- по категориям (название по алфавиту), 
    sortedPhoto     :: [a] -> m [a] -- по количеству фотографий
