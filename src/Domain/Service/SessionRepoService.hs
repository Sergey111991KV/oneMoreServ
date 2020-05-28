module Domain.Service.SessionRepoService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except
import Katip

import Domain.ImportEntity
import Domain.Service.CommonService 

class Monad m => SessionRepo m where
    findUsers                   :: Login -> Password -> m (Maybe Users)
    findUserIdByUser            :: Users -> m (Maybe UserId)
    newSession                  :: UserId -> m SessionId
    findUserIdBySessionId       :: SessionId -> m (Maybe UserId)
    findAccessAdminByUserId     :: UserId -> m (Maybe Bool)
    -- findAccessAuthorByUserId    :: UserId -> m (Maybe Bool)
   

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

