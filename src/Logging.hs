module Logging where

import ClassyPrelude
import Data.Aeson
import GHC.Generics
import Domain.Types.Errors as E
import System.IO
import System.IO.Unsafe
import Data.Text.Time


type LoggingInConfig = Logging

data LogConfig = LogConfig
  { logFile :: FilePath
  , logLevelForFile :: LoggingInConfig
  , logConsole :: Bool
  } deriving (Show, Generic)

type LogForFile =  Logging 

data Logging
  = Debug
  | Warning
  | Error
  deriving (Eq, Ord, Read, Show, Generic)


takeValueLogging :: Logging -> Integer
takeValueLogging log
                | log == Debug     = 1
                | log == Warning   = 2
                | log == Logging.Error     = 3

caseOfWriteLogging :: Logging -> LogForFile -> Bool
caseOfWriteLogging log logConf =  (takeValueLogging log) >= (takeValueLogging logConf)

instance ToJSON Logging

instance FromJSON Logging


instance ToJSON LogConfig

instance FromJSON LogConfig

writeInLogFile :: FilePath -> Bool -> String -> IO ()
writeInLogFile lF bl txt = do
        case bl of
            True -> appendFile lF txt
            False -> return ()

writeInTerminal :: Bool -> String -> IO ()
writeInTerminal bl txt = do
        case bl of
                True -> System.IO.print txt
                False -> return ()

 
writFileHandler ::  FilePath -> LoggingInConfig -> Logging -> Bool -> String -> IO ()
writFileHandler lF logCong log bl txt = do
        let ifWrite =  caseOfWriteLogging logCong log
        writeInLogFile lF ifWrite txt
        writeInTerminal bl txt  

writeTextError      :: E.Error -> String -> String
writeTextError  err txt    =  date ++  errText ++  txt
                where
                        date    = takeCurrentDate
                        errText = errorString err
      

writeText :: String ->  String
writeText txt = date ++  txt
        where
                date    = takeCurrentDate

        
takeCurrentDate ::  String
takeCurrentDate = unpack  $ formatISODateTime $ unsafePerformIO time
        where 
                time = getCurrentTime
     
writeLogginHandler :: LogConfig -> Logging -> String -> IO ()
writeLogginHandler  (LogConfig lf logLev logBool) loging txt =  writFileHandler lf logLev loging logBool txt

