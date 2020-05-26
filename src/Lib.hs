module Lib
    ( mainL
    ) where

-- import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.ImportPostgres as PG
import Domain.ImportService
import Domain.ImportEntity
import ClassyPrelude
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Katip

type State = (PG.State)
newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, KatipContext, Katip, MonadThrow)
run :: LogEnv -> State -> App a -> IO a
run le state = runKatipContextT le () mempty . flip runReaderT state . unApp


instance SessionRepo App where
  newSession = PG.newSession
  findUserIdByUser = PG.findUserIdByUser
  findUserIdBySessionId = PG.findUserIdBySessionId

-- instance CommonService App where
--       create  =   PG.create
--       editing =   PG.editing
--       getAll  =   PG.getAll
--       getOne  =   PG.getOne
--       remove  =   PG.remove


mainL :: IO ()
mainL = do
    print "dsaf"
-- mainL :: IO ()
-- mainL = withKatip $ \le -> do 
--   mState <- newTVarIO M.initialState
--   PG.withState pgCfg $ \pgState -> run le (pgState, mState) action
--   where
--     pgCfg = PG.Config
--             { PG.configUrl = "postgresql://localhost/hauth"
--             , PG.configStripeCount = 2
--             , PG.configMaxOpenConnPerStripe = 5
--             , PG.configIdleConnTimeout = 10
--             }

-- action :: App ()
-- action = do
--     let email = either undefined id $ mkLogin "oleg@test.com"
--         passw = either undefined id $ mkPassword "5678ABCDefgh"
--         auth = Auth email passw Admins
--     session <-  login auth
--     case session of
--       Left err -> print "non found user"
--       Right s ->  print (s, authAccess auth)
 

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv