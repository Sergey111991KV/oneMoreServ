module Domain.Types.AccesTypes where

import ClassyPrelude 
import Domain.Types.Imports
import Domain.Validation.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except
import Katip
import GHC.Generics
import Database.PostgreSQL.Simple.FromField (FromField, fromField)


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
  , authAdmin    :: Bool
  } deriving (Show, Eq, Generic)

instance FromRow Users where
  fromRow = Users <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromJSON Users
instance ToJSON Users
instance  ToRow Users



newtype Login = Login { loginRaw :: Text } deriving (Show, Eq, Generic)

rawLogin :: Login -> Text
rawLogin = loginRaw

instance FromField Login where
    fromField field mb_bytestring = Login <$> fromField field mb_bytestring

instance  ToField Login
instance FromJSON Login
instance ToJSON Login
instance ToRow Login

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq, Generic)

rawPassword :: Password -> Text
rawPassword = passwordRaw

instance FromField Password where
    fromField field mb_bytestring = Password <$> fromField field mb_bytestring

instance  ToField Password
instance FromJSON Password
instance ToJSON Password
instance  ToRow Password
                  

newtype UserId = UserId Int deriving (Generic, Show, Eq, Ord)

instance FromField UserId where
  fromField field mb_bytestring = UserId <$> fromField field mb_bytestring
instance FromRow UserId where
  fromRow = UserId <$> field

instance FromJSON UserId
instance ToJSON UserId
instance  ToRow UserId
instance ToField UserId



type SessionId = Text

data LoginError = LoginError deriving (Show, Eq)

          
class Monad m => SessionRepo m where
    findUsers               :: Login -> Password -> m (Maybe Users)
    findUserIdByUser        :: Users -> m (Maybe UserId)
    newSession              :: UserId -> m SessionId
    findUserIdBySessionId   :: SessionId -> m (Maybe UserId)
   

            --- Katip

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

            ---  Loging
            
resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId
          
login :: (KatipContext m, SessionRepo m) => Login -> Password -> m (Either LoginError SessionId)
login log pass = runExceptT $ do
  resultFirst  <- lift $ findUsers log pass
  case resultFirst of
    Nothing -> throwError LoginError
    Just auth -> do
      resultSecond <- lift $ findUserIdByUser auth
      case resultSecond of
        Nothing -> throwError LoginError
        Just uId -> withUserIdContext uId . lift $ newSession uId




                                --  create Auth



-- data EmailValidationErr = EmailValidationErrInvalidEmail

-- data RegistrationError
--   = RegistrationErrorEmailTaken
--   deriving (Show, Eq)

-- data PasswordValidationErr = PasswordValidationErrLength Int
--   | PasswordValidationErrMustContainUpperCase
--   | PasswordValidationErrMustContainLowerCase
--   | PasswordValidationErrMustContainNumber

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