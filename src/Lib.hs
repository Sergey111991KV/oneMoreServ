module Lib
    ( mainL
    ) where

-- import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.ImportPostgres as PG
import Domain.ImportService
import Domain.ImportEntity
import ClassyPrelude
import Control.Monad.Catch (MonadThrow, MonadCatch)
import qualified Adapter.HTTP.Main as HTTP

type State = (PG.State)
newtype App a = App
  { unApp :: ReaderT State IO a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadThrow)

run :: State -> App a -> IO a
run  state =  flip runReaderT state . unApp


instance SessionRepo App where
  newSession              = PG.newSession
  findUserIdByUser        = PG.findUserIdByUser
  findUserIdBySessionId   = PG.findUserIdBySessionId
  findUsers               = PG.findUsers
  findAccessAdminByUserId = PG.findAccessAdminByUserId
  newUserId               = PG.newUserId


  -- findAccessAuthorByUserId = PG.findAccessAuthorByUserId
instance CommonService App where
      create  =   PG.create
      editing =   PG.editing
      getAll  =   PG.getAll
      getOne  =   PG.getOne
      remove  =   PG.remove


instance SearchIn App where
    inContent =  PG.inContent
    -- inEntyty  = PG.inEntyty


instance FilterService App where
    filterOfData        =  PG.filterOfData  
    filterAuthor        =  PG.filterAuthor
    -- filterCategory      =  PG.filterCategory
    -- filterTeg           =  PG.filterTeg
   

mainL :: IO ()
mainL = do
    withState $ \port  state -> do
      let runner = run  state
      HTTP.mainALL port runner
      
    
    
 

-- withKatip :: (LogEnv -> IO a) -> IO a
-- withKatip app =
--   bracket createLogEnv closeScribes app
--   where
--     createLogEnv = do
--       logEnv <- initLogEnv "HAuth" "prod"
--       stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
--       registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv
      
      
withState :: (Int -> State -> IO ()) -> IO ()
withState action =  do
    PG.withState pgCfg $ \pgState -> do
          let state = pgState
          action port state
  where
    pgCfg = 
      PG.Config 
      -- "host='localhost' port=5431 dbname='hblog'" 
      -- user = пароль = b host = localhost port = 5432 dbname = c sslmode = disable
      -- " postgresql: // localhost: 5431 / postgres ""
      { PG.configUrl = " host='localhost' port=5431 dbname='hblog'"
      , PG.configStripeCount = 2
      , PG.configMaxOpenConnPerStripe = 5
      , PG.configIdleConnTimeout = 10
      }
    port = 3000