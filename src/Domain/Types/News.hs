module Domain.Types.News where

import Domain.Types.Imports
import ClassyPrelude


data News = News {
    id_news               :: Int,
    data_create_news      :: UTCTime,
    authors_id_news       :: Int,
    category_3_id_news    :: Int,
    text_news             :: String,
    main_photo_url_news   :: String,
    other_photo_url_news  :: String,
    short_name_news       :: String
    } deriving (Eq, Ord, Show, Read, Generic)

instance FromRow News where
    fromRow = News <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 
    
instance FromJSON News
instance ToJSON News
instance  ToRow News