module Examples.ExampFromField where

        

import ClassyPrelude
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Data.Time
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Data.Has
import Data.Pool
import Data.Text.Time
import qualified Data.Text as Text
import qualified Data.ByteString as B

import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E


-- fromField :: FieldParser a
-- type FieldParser a = Field -> Maybe ByteString -> Conversion a
-- Идентификаторы объектов (OID)
-- getTypeInfo :: Connection -> Oid -> IO TypeInfo



-- optionalField :: FieldParser a -> FieldParser (Maybe a)
-- optionalField p f mv =
--     case mv of
--       Nothing -> pure f Nothing
--       Just _  -> Just <$> p f mv
-- instance (FromField a, FromField b) => FromField (Either a b) where
--     fromField f dat =   (Right <$> fromField f dat)
                    -- <|> (Left  <$> fromField f dat)