module Examples.ExampleJSON where

        

import ClassyPrelude
import Data.Aeson 
import Data.ByteString.Lazy as LB

data Address = Address
    { house  :: Integer
    , street :: String
    , city   :: String
    , state  :: String
    , zip    :: Integer
    } deriving (Show,  Generic)

data Person = Person
    { name    :: String
    , age     :: Integer
    , address :: Address
    } deriving (Show, Generic)

instance ToJSON Person
instance FromJSON Person

-- instance ToJSON Person where
--     toEncoding = genericToEncoding defaultOptions

instance ToJSON Address
instance FromJSON Address

aa :: LB.ByteString
aa = "{\"name\": \"some body\", \"age\" : 23, \"address\" : {\"house\" : 285, \"street\" : \"7th Ave.\", \"city\" : \"New York\", \"state\" : \"New York\", \"zip\" : 10001}}"
 
mainExample :: IO ()
mainExample = print $  show (decode aa :: Maybe Person)



-- Иногда мы хотим, чтобы некоторые поля в строке JSON были необязательными. Например,

-- data Person = Person { firstName :: Text
--                      , lastName  :: Text
--                      , age       :: Maybe Int 
--                      }
-- Это может быть достигнуто

-- import Data.Aeson.TH

-- $(deriveJSON defaultOptions{omitNothingFields = True} ''Person)


-- :l Examples.ExampleJSON mainExample