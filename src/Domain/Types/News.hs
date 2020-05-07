module Domain.Types.News where

import Domain.Types.Imports
import ClassyPrelude


data News = News {
    short_name_news       :: String,
    data_create_news      :: UTCTime,
    -- ZonedTime,
    authors_id_news       :: Int,
    category_3_id_news    :: Int,
    text_news             :: String,
    main_photo_url_news   :: String,
    other_photo_url_news  :: String,
    id_news               :: Int
    } deriving (Eq, Ord, Show, Read)

instance FromRow News where
    fromRow = News <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field 
   