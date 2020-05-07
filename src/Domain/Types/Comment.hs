module Domain.Types.Comment where

import Domain.Types.Imports
import ClassyPrelude



data Comment = Comment {
    id_comments         :: Int,
    text_comments        :: String,
    data_create_comments :: UTCTime,
    -- ZonedTime,
    news_id_comments     :: Int,              
    users_id_comments    :: Int
    } deriving (Show)

instance FromRow Comment where
    fromRow = Comment <$> field <*> field <*> field <*> field <*> field
      