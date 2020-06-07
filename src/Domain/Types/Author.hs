module Domain.Types.Author where

import Domain.Types.Imports
import ClassyPrelude
import Database.PostgreSQL.Simple.FromField 



data Author  = Author {
    id_author          :: Int,
    description :: String,
    user_id     :: Int
    } deriving (Show, Eq, Generic)

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

instance FromJSON Author
instance ToJSON Author
instance  ToRow Author
instance FromField Author
  


-- instance FromField Author  where
--   fromField field mb = do
--       v <-  fromField field mb
--       valueToProductProperties v
--       where
--                  valueToProductProperties :: Value -> Conversion Author
--                  valueToProductProperties v = case fromJSON v of
--                     Success a -> return a
--                     Error err -> returnError ConversionFailed field "Cannot parse product properties"

-- newtype TenantId = TenantId Int deriving(Show)

-- data TenantStatus = TenantStatusActive | TenantStatusInActive | TenantStatusNew
--   deriving (Show)

-- data TenantPoly key name fname lname email phone status b_domain = Tenant
--   { tenant_id               :: key
--   , tenant_name             :: name
--   , tenant_firstname        :: fname
--   , tenant_lastname         :: lname
--   , tenant_email            :: email
--   , tenant_phone            :: phone
--   , tenant_status           :: status
--   , tenant_backofficedomain :: b_domain
--   } deriving (Show)

-- type Tenant = TenantPoly TenantId Text Text Text Text Text TenantStatus Text


-- instance FromField ProductProperties where
--   fromField field mb = do
--     v <-  fromField field mb
--     valueToProductProperties v
--     where
--       valueToProductProperties :: Value -> Conversion ProductProperties
--       valueToProductProperties v = case fromJSON v of
--         Success a -> return a
--         Error err -> returnError ConversionFailed field "Cannot parse product properties"


-- data ProductProperties = ProductProperties { product_color :: String, product_weight :: String} deriving (Show)