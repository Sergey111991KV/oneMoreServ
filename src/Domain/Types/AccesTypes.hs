module Domain.Types.AccesTypes where

import Domain.Types.Imports
import qualified Text.Digestive.Form as DF 
import Domain.Validation.Validation

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




data State = State
  { stateAuths :: [(UserId, Auth)]
--   , stateUserIdCounter :: Int
  , stateSessions :: Map SessionId UserId
  } deriving (Show, Eq)

initialState :: State
initialState = State 
  { stateAuths = []
--   , stateUserIdCounter = 0
  , stateSessions = mempty
  }


