module Domain.Types.AccesTypes where

import ClassyPrelude 
import Domain.Types.Imports
import Domain.Validation.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except
import Katip
import GHC.Generics
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.TypeInfo as TI
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           Database.PostgreSQL.Simple.TypeInfo.Macro as TI
import           Data.ByteString.Builder

data  Users = Users
  { 
    id_user      :: UserId
  , name         :: String
  , lastName     :: String
  , authLogin    :: Login
  , authPassword :: Password
  , avatar       :: String
  , dataCreate   :: UTCTime
  , authAuthor   :: Bool
  , authAdmin    :: AccessAdmin
  } deriving (Show, Eq, Generic)

instance FromRow Users where
  fromRow = Users <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromJSON Users
instance ToJSON Users
-- instance  ToRow Users



newtype Login = Login { loginRaw :: Text } deriving (Show, Eq, Generic)

rawLogin :: Login -> Text
rawLogin = loginRaw

instance FromField Login where
    fromField field mb_bytestring = Login <$> fromField field mb_bytestring

-- instance  ToField Login
instance FromJSON Login
instance ToJSON Login
instance ToRow Login

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq, Generic)

rawPassword :: Password -> Text
rawPassword = passwordRaw

instance FromField Password where
    fromField field mb_bytestring = Password <$> fromField field mb_bytestring

-- instance  ToField Password
instance FromJSON Password
instance ToJSON Password
instance  ToRow Password
                  

newtype UserId = UserId {rawUserId :: Int} deriving (Generic, Show, Eq, Ord)

instance FromField UserId where
  fromField field mb_bytestring = UserId <$> fromField field mb_bytestring
instance FromRow UserId where
  fromRow = UserId <$> field

instance FromJSON UserId
instance ToJSON UserId
instance  ToRow UserId

instance ToField UserId where
  toField ip = toField $ rawUserId ip

 
data AccessAdmin = AccessAdmin  Bool deriving (Generic, Show, Eq, Ord)
rawAccessAdmin :: AccessAdmin -> Bool
rawAccessAdmin (AccessAdmin a) = a
instance FromRow AccessAdmin where
  fromRow = AccessAdmin <$> field
instance FromJSON AccessAdmin
instance ToJSON AccessAdmin
instance  ToRow AccessAdmin
instance ToField AccessAdmin where
  toField (AccessAdmin True)  = Plain (byteString "true")
  toField (AccessAdmin False) = Plain (byteString "false")

instance FromField AccessAdmin where
      fromField f bs
        | typeOid f /= $(inlineTypoid TI.bool) = returnError Incompatible f ""
        | bs == Nothing                 = returnError UnexpectedNull f ""
        | bs == Just "t"                = pure (AccessAdmin True)
        | bs == Just "f"                = pure (AccessAdmin False)
        | otherwise                     = returnError ConversionFailed f ""

newtype SessionId = SessionId { sessionRaw :: Text } deriving (Generic, Show, Eq, Ord)
rawSession :: SessionId -> Text
rawSession = sessionRaw
instance FromField SessionId where
  fromField field mb_bytestring = SessionId <$> fromField field mb_bytestring
instance FromRow SessionId where
  fromRow = SessionId <$> field
instance FromJSON SessionId
instance ToJSON SessionId
instance  ToRow SessionId
instance ToField SessionId where
  toField ip = toField $ sessionRaw ip

data LoginError = LoginError deriving (Show, Eq)


                                --  create Auth



-- data EmailValidationErr = EmailValidationErrInvalidEmail

-- data RegistrationError
--   = RegistrationErrorEmailTaken
--   deriving (Show, Eq)

-- data PasswordValidationErr = PasswordValidationErrLength Int
--   | PasswordValidationErrMustContainUpperCase
--   | PasswordValidationErrMustContainLowerCase
--   | PasswordValidationErrMustContainNumber
mkSessionId :: Text -> Either [ErrMsg] SessionId
mkSessionId =  validate SessionId 
                  [
                    regexMatches [re|\d|] "Something was wrong in sessionId"
                  ]

mkLogin :: Text -> Either [ErrMsg] Login
mkLogin =
            validate Login
              [ regexMatches
                  [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
                  "Not a valid login"
              ]


mkPassword :: Text -> Either [ErrMsg] Password
mkPassword =
                  validate Password
                    [ lengthBetween 5 50 "Should between 5 and 50"
                    , regexMatches [re|\d|] "Should contain number"
                    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
                    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
                    ]
    