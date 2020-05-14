
module Domain.Types.UserB where

import Domain.Types.Imports
import ClassyPrelude

data UserB = UserB { 
    userId_user      :: Int,
    name_user        :: String,
    password_user    :: String,
    second_name_user :: String,
    data_create_user :: UTCTime,
    -- ZonedTime,
    admin_user       :: Bool,
    avatar_user      :: String
    } deriving (Show) 

instance FromRow UserB where
    fromRow = UserB <$> field <*> field <*> field  <*> field  <*> field  <*> field  <*> field 


