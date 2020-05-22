module Domain.Service.SortedOfService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 
import Domain.Service.FilterService 

class (FilterService a, CommonService a) => SortedOf a where        --     API новостей должно поддерживать сортировку по:
    sortedDate      :: Day -> [a]           -- дате,
    sortedAuthor    :: Author -> [a]        -- автору (имя по алфавиту),
    sortedCategory  :: [a] -> [a] -- по категориям (название по алфавиту), 
    sortedPhoto     :: [a] -> [a] -- по количеству фотографий
