module Domain.Types.AccesTypes where

import Domain.Types.Imports

import ClassyPrelude

data Access = Users | Authors | Admins deriving (Show, Eq)

newtype UserId    = UserId Int
newtype UserToken = UserToken ByteString

data User = User
  { user_id :: UserId,
    access  :: Access
  }

data Login = Login
  { login_userId :: UserId,
    login_userToken :: UserToken
  }