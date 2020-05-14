module Adapter.PostgreSQL.APIEntityes.APIPosCategory where

import ClassyPrelude
import Database.PostgreSQL.Simple
import Adapter.PostgreSQL.APIConnection
import Domain.ImportEntity as IE
import Domain.Service.CommonService

create  :: PG r m =>  Maybe Category -> Either IE.Error Category
create m = do
    case m of
        Nothing -> Left IE.DataError
        Just auth -> Right auth

