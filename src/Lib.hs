module Lib
    ( someFunc
    ) where

import Adapter.HTTP.Main 
import ClassyPrelude

-- runApp :: Env -> App a -> IO a
-- runApp = flip runReaderT

someFunc :: IO ()
someFunc = do
    print "dsf"
    -- let runner = runApp env 
    mainH 3000 id
