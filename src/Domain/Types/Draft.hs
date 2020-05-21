module Domain.Types.Draft where

import Domain.Types.Imports
import ClassyPrelude



data Draft = Draft {
    id_draft          :: Int,
    text_draft        :: String,
    news_id_draft      :: Int,
    data_create_draft  :: UTCTime
    --  ZonedTime
    } deriving (Show)

instance FromRow Draft where
    fromRow = Draft <$> field <*> field <*> field <*> field 
      