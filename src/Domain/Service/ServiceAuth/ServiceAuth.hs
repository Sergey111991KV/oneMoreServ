module Domain.Service.ServiceAuth.ServiceAuth where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Domain.Validation.Validation 
import Text.Regex.PCRE.Heavy
import Domain.Service.CommonService 

resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

initialState :: State
initialState = State 
  { stateAuths = []
--   , stateUserIdCounter = 0
  , stateSessions = mempty
  }


mkPassword :: Text -> Either [ErrMsg] Password
mkPassword =
        validate Password
          [ lengthBetween 5 50 "Should between 5 and 50"
          , regexMatches [re|\d|] "Should contain number"
          , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
          , regexMatches [re|[a-z]|] "Should contain lowercase letter"
          ]

mkLogin :: Text -> Either [ErrMsg] Login
mkLogin =
  validate Login
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid login"
    ]

