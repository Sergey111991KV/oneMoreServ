module Adapter.InMemory.Auth where

import Domain.ImportEntity as E
import ClassyPrelude
import Data.Has
import Text.StringRandom


-- idd = E.UserId 1

-- data State = State
--   { stateAuths :: [(E.UserId, E.Auth)]
--   , stateUserIdCounter :: Int
--   , stateSessions :: Map E.SessionId E.UserId
--   } deriving (Show, Eq)

-- initialState :: State
-- initialState = State
--     { stateAuths =  [
--           ( UserId 1, Auth
--             (either undefined id $ mkLogin "sergey@test.com")
--             (either undefined id $ mkPassword "1234ABCDefgh")
--             Admins
--           ),
--           ( UserId 2, Auth
--             (either undefined id $ mkLogin "dasha@test.com")
--             (either undefined id $ mkPassword "1234ABCDefgh")
--             Authors
--           ),
--           (UserId 3 , Auth
--             (either undefined id $ mkLogin "victor@test.com")
--             (either undefined id $ mkPassword "1234ABCDefgh")
--             Users
--           )
--         ]
--     , stateUserIdCounter = 0
--     , stateSessions = mempty
--     }


-- type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

-- findUserIdByAuth    :: InMemory r m 
--                     => Auth -> m (Maybe UserId)
-- findUserIdByAuth auth = do
--     tvar <- asks getter
--     state <- liftIO $ readTVarIO tvar
--     let mayUserId = map fst . find ((auth ==) . snd) $ stateAuths state
--     case mayUserId of
--       Nothing  -> return Nothing
--       Just uId -> return $ Just uId
      
-- newSession          :: InMemory r m 
--                     => UserId -> m SessionId
-- newSession uId = do
--     tvar <- asks getter
--     sId <- liftIO $ ((tshow uId) <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
--     atomically $ do --- Атомарная (атом от греч. atomos — неделимое) операция — операция, 
--                     --- которая либо выполняется целиком, либо не выполняется вовсе; операция, 
--                     --- которая не может быть частично выполнена и частично не выполнена.
--       state <- readTVar tvar
--       let sessions = stateSessions state
--           newSessions = insertMap sId uId sessions
--           newState = state { stateSessions = newSessions }
--       writeTVar tvar newState
--       return sId


-- findUserIdBySessionId :: InMemory r m
--                       => E.SessionId -> m (Maybe E.UserId)
-- findUserIdBySessionId sId = do
--   tvar <- asks getter -- what is getter fuck?? -- I get simple undestand about environment where getter take t -> a, а также это сразу прописывается в ограничении
--   liftIO $ lookup sId . stateSessions <$> readTVarIO tvar




  