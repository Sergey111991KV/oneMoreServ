module Domain.Types.AccesTypes where

import Domain.Types.Imports
import ClassyPrelude

data Access = Users | Authors | Admins deriving (Show, Eq)

type  SessionId  =  Text

type UserId = Int

newtype   Login = Login { rawLogin :: Text } deriving (Show, Eq, Ord)

newtype Password = Password { rawPassword :: Text } deriving (Show, Eq)

-- type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data Auth = Auth
  { authLogin :: Login
  , authPassword :: Password
  } deriving (Show, Eq)





