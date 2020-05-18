module Lib
    ( mainL
    ) where

import qualified Adapter.InMemory.Auth as M
import Domain.ImportEntity
import ClassyPrelude
import Katip

type State = TVar M.State
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip)
run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp


instance SessionRepo App where
  newSession = M.newSession
  findUserIdByAuth = M.findUserIdByAuth
  findUserIdBySessionId = M.findUserIdBySessionId

mainL :: IO ()
mainL = withKatip $ \le -> do 
  state <- newTVarIO M.initialState
  run le state action

action :: App ()
action = do
    let email = either undefined id $ mkLogin "sergey@test.com"
        passw = either undefined id $ mkPassword "1234ABCDefgh"
        auth = Auth email passw Admins
    session <-  login auth
    case session of
      Left err -> print "non found user"
      Right s ->  print (s, authAccess auth)
 

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv