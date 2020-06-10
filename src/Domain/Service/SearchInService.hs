module Domain.Service.SearchInService where

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