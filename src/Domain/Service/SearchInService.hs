module Domain.Service.SearchInService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip

import Domain.Service.CommonService 

class CommonService m  => SearchIn m  where
    inContent :: a -> m [a]   -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
    inEntyty  :: a -> m [a]   --  либо в имени автора, либо в названии категории/тега