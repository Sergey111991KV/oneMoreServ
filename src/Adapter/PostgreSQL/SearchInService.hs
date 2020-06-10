module Adapter.PostgreSQL.SearchInService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except
import Katip
import Database.PostgreSQL.Simple
import Data.Maybe
import Data.Text.Time


import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E
import Adapter.PostgreSQL.CommonPostgres as CP

-- class CommonService m  => SearchIn m  where
--     inContent :: Text -> m (Either E.Error [E.News] )   -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
--     inEntyty  :: Text -> m (Either E.Error [E.News] )    --  либо в имени автора, либо в названии категории/тега


inContent  :: PG r m =>  Text -> m (Either E.Error [E.News] )
inContent = undefined