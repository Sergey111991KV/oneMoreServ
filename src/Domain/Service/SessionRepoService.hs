module Domain.Service.SessionRepoService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except


import Domain.ImportEntity
import Domain.Service.CommonService 

class Monad m => SessionRepo m where
    findUsers                   :: Login -> Password -> m (Maybe Users)
    findUserIdByUser            :: Users -> m (Maybe UserId)
    newSession                  :: UserId -> m SessionId
    findUserIdBySessionId       :: SessionId -> m (Maybe UserId)
    findAccessAdminByUserId     :: UserId -> m (Maybe AccessAdmin)
    newUserId                   :: m UserId
    -- findAccessAuthorByUserId    :: UserId -> m (Maybe Bool)
    

            --- Katip

withUserIdContext :: UserId -> m a -> m a
withUserIdContext uId = undefined

            ---  Loging
            
resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId
          
login :: SessionRepo m => Login -> Password -> m (Either LoginError SessionId)
login log pass = runExceptT $ do
  resultFirst  <- lift $ findUsers log pass
  case resultFirst of
    Nothing -> throwError LoginError
    Just auth -> do
      resultSecond <- lift $ findUserIdByUser auth
      case resultSecond of
        Nothing -> throwError LoginError
        Just uId -> withUserIdContext uId . lift $ newSession uId



findNewUserIdFromMonad :: SessionRepo m => m UserId -> UserId
findNewUserIdFromMonad = undefined
