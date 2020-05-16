module Adapter.InMemory.Auth where
  
import ClassyPrelude
import Text.StringRandom
import Data.Has

import Domain.ImportService
import Domain.ImportEntity



type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

-- State


data State = State
  { stateAuths :: [(UserId, Auth)]
  , stateUserIdCounter :: Int
  , stateSessions :: Map SessionId UserId
  } deriving (Show, Eq)

initialState :: State
initialState = State 
  { stateAuths = []
  , stateUserIdCounter = 0
  , stateSessions = mempty
  }



-- Session

newSession :: InMemory r m
           => UserId -> m SessionId
newSession uId = do
          tvar <- asks getter
          sId <- liftIO $ ((tshow uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
          atomically $ do
            state <- readTVar tvar
            let sessions = stateSessions state
                newSessions = insertMap sId uId sessions
                newState = state { stateSessions = newSessions }
            writeTVar tvar newState
            return sId
        

findUserIdBySessionId :: InMemory r m
            => SessionId -> m (Maybe UserId)
findUserIdBySessionId sId = do
          tvar <- asks getter
          liftIO $ lookup sId . stateSessions <$> readTVarIO tvar
