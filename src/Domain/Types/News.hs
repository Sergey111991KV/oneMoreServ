module Domain.Types.News where

import Domain.Types.Imports

import ClassyPrelude
import           Data.ByteString.Builder
import Database.PostgreSQL.Simple.FromField 
-- import           Opaleye
import Database.PostgreSQL.Simple.Types 


data News = News {
    id_news               :: Int,
    data_create_news      :: UTCTime,
    authors_id_news       :: Int,
    category_3_id_news    :: Int,
    text_news             :: Text,
    main_photo_url_news   :: Text,
    other_photo_url_news  :: PGArray Text,
    short_name_news       :: Text
    } deriving (Eq, Ord, Show, Read, Generic)

instance FromRow News where
    fromRow = News <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 
    

instance  ToRow News where
    toRow news = [
                    toField (id_news news), 
                    toField (data_create_news news), 
                    toField (authors_id_news news),
                    toField (category_3_id_news news), 
                    toField (text_news news), 
                    toField (main_photo_url_news news),
                    toField (other_photo_url_news news), 
                    toField (short_name_news news)
                ]


instance FromJSON News
instance ToJSON News
-- instance FromField News where
--     fromField = undefined
--      fromField :: FieldParser Int)  = (News <$>)  
--             . (fromField :: FieldParser Int) 
--             <*> (fromField :: FieldParser UTCTime) 
--             <*> (fromField :: FieldParser Int)
--             <*> (fromField :: FieldParser Int)
--             <*> (fromField :: FieldParser Text)
--             <*> (fromField :: FieldParser Text)
--             <*> (fromField :: FieldParser (PGArray Text))
--             <*> (fromField :: FieldParser Text)

-- instance FromJSON (PGArray Text)
-- instance FromJSON (PGArray Text)
-- instance FromJSON (PGArray Text)
-- instance FromJSON (PGArray Text)
-- instance FromField (PGArray Text) where
--     fromField = undefined


instance FromJSON (PGArray Text)
instance ToJSON (PGArray Text)
-- instance ToField (PGArray Text) where
--         toField pgArray =
--                 case fromPGArray pgArray of
--                         [] -> Plain (byteString "'{}'")
--                         xs -> Many $
--                           Plain (byteString "ARRAY[") :
--                           (intersperse (Plain (char8 ',')) . map toField $ xs) ++
--                           [Plain (char8 ']')]

-- deriving instance Show (PGArray Text) => Show (PGArray Text)
-- deriving instance Eq (PGArray Text) => Eq (PGArray Text)
-- deriving instance Ord (PGArray Text) => Ord (PGArray Text)
-- deriving instance Read (PGArray Text) => Read (PGArray Text)
deriving instance Generic (PGArray Text) => Generic (PGArray Text)

-- instance FromField [Text]
-- instance  ToField [Text] where
--     toField [] = Plain $ byteString "(null)"
--     toField xs = Many $ Plain (char8 '(') : (intersperse (Plain (char8 ',')) . map toField $ xs) ++ [Plain (char8 ')')] 

-- newtype PhotoArray Text = PGArray {fromPGArray :: [Text]} deriving (Eq, Ord, Read, Show, Typeable, Functor)
-- instance (FromField Text, Typeable Text) => FromField  [Text] where
--         fromField = arrayTextPars fromField

-- arrayTextPars  ::  Typeable  Text =>  FieldParser  Text  ->  FieldParser [Text] 
-- arrayTextPars = undefined
  


-- newtype PGArray a = PGArray {fromPGArray :: [a]} deriving (Eq, Ord, Read, Show, Typeable, Functor)