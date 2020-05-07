
module Domain.Types.User where

import Domain.Types.Imports
import ClassyPrelude

data User = User { 
    userId_user      :: Int,
    name_user        :: String,
    second_name_user :: String,
    data_create_user :: UTCTime,
    -- ZonedTime,
    admin_user       :: Bool,
    avatar_user      :: String
    } deriving (Show) 

instance FromRow User where
    fromRow = User <$> field <*> field  <*> field  <*> field  <*> field  <*> field 


