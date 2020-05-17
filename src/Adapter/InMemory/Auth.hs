module Adapter.InMemory.Auth where

import Domain.ImportEntity as E
import ClassyPrelude
import Data.Has


data State = State
  { stateAuths :: [(E.UserId, E.Auth)]
  , stateAcces :: [(E.Access, E.Auth)]
  , stateUserIdCounter :: Int
  , stateSessions :: Map E.SessionId E.UserId
  } deriving (Show, Eq)

initialState :: State
initialState = State
    { stateAuths = []
    , stateAcces = []
    , stateUserIdCounter = 0
    , stateSessions = mempty
    }


type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

findUserIdByAuth    :: InMemory r m => Auth -> m (Maybe UserId)
findUserIdByAuth = undefined

newSession          :: InMemory r m => UserId -> m SessionId
newSession = undefined

findAccessByAuth    :: InMemory r m => Auth -> m Access
findAccessByAuth = undefined

findUserBySessionId :: InMemory r m => SessionId -> m (Maybe UserId)
findUserBySessionId = undefined
