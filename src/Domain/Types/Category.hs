module Domain.Types.Category where

import Domain.Types.Imports
import ClassyPrelude


data Category1 = Category1 
    {
        id_1            :: Int,
        name_1          :: String
    } deriving (Show)

data Category2 = Category2
    {
        id_2            :: Int,
        name_2          :: String,
        category_1_id   :: Int
    } deriving (Show)
    
    
data Category3 = Category3
    {
        id_3            :: Int,
        name_3          :: String,
        category_2_id   :: Int
    } deriving (Show)

instance FromRow Category1 where
    fromRow = Category1 <$> field <*> field 
      
instance FromRow Category2 where
    fromRow = Category2 <$> field <*> field <*> field

instance FromRow Category3 where
    fromRow = Category3 <$> field <*> field <*> field
      