module Adapter.PostgreSQL.APIEntityes.APIPosAuthor where

import ClassyPrelude
import Database.PostgreSQL.Simple
import Adapter.PostgreSQL.APIConnection
import Domain.ImportEntity as IE
import Domain.Service.CommonService


-- create  :: PG r m =>  Maybe Author -> Either IE.Error Author
-- create m = do
--     case m of
--         Nothing -> Left IE.DataError
--         Just auth -> Right auth
-- -- listArticles :: Pool Connection -> IO [Article]

-- createAuthor :: IO Int -> St
-- createAuthor i n d = Author {
--                 unsafeDupablePerformIO
--         }   
-- newIdAutor :: Connection -> IO Int
-- newIdAutor pool = do
--         [Only i] <- query_ pool "SELECT MAX(id) FROM authors;"
--         return (i + 1)