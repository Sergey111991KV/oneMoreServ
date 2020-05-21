module Domain.Types.Category where

import Domain.Types.Imports
import ClassyPrelude


data Category1 = Category1 
    {
        id_1            :: Int,
        name_1          :: String
    } deriving (Show, Generic)

instance FromRow Category1 where
  fromRow = Category1 <$> field <*> field
  
instance FromJSON Category1
instance ToJSON Category1
instance  ToRow Category1

data Category2 = Category2
    {
        id_2            :: Int,
        name_2          :: String,
        category_1_id   :: Int
    } deriving (Show, Generic)

instance FromRow Category2 where
  fromRow = Category2 <$> field <*> field <*> field
  
instance FromJSON Category2
instance ToJSON Category2
instance  ToRow Category2
    
data Category3 = Category3
    {
        id_3            :: Int,
        name_3          :: String,
        category_2_id   :: Int
    } deriving (Show, Generic)

instance FromRow Category3 where
  fromRow = Category3 <$> field <*> field <*> field
  
instance FromJSON Category3
instance ToJSON Category3
instance  ToRow Category3


      