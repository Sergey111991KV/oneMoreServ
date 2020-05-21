module Domain.Types.AccesTypes where

import ClassyPrelude 
import Domain.Types.Imports
import Domain.Validation.Validation
import Text.Regex.PCRE.Heavy
import Control.Monad.Except
import Katip
import Data.Aeson
import GHC.Generics

data  User = User
  { authLogin    :: Login
  , authPassword :: Password
  , authAuthor   :: Bool
  , authAdmin    :: Bool
  } deriving (Show, Eq)

  

newtype Login = Login { loginRaw :: Text } deriving (Show, Eq)

rawLogin :: Login -> Text
rawLogin = loginRaw

newtype Password = Password { passwordRaw :: Text } deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw


newtype UserId = UserId Int deriving (Generic, Show, Eq, Ord)

instance ToJSON UserId where 

type SessionId = Text

data LoginError = LoginError deriving (Show, Eq)

          
class Monad m => SessionRepo m where
    findUser                :: Login -> Password -> m (Maybe User)
    findUserIdByUser        :: User -> m (Maybe UserId)
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
  resultFirst  <- lift $ findUser log pass
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