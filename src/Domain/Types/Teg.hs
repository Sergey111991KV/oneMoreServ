module Domain.Types.Teg where

import Domain.Types.Imports
import ClassyPrelude

data Teg = Teg {
    id_teg   :: Int,
    name_teg :: String
    } deriving (Show, Eq, Generic)

instance FromRow Teg where
    fromRow = Teg <$> field <*> field 

  
instance FromJSON Teg
instance ToJSON Teg
instance  ToRow Teg