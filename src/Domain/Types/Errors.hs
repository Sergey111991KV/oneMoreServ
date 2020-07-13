module Domain.Types.Errors where

import Domain.Types.Imports
import ClassyPrelude

data Error = AccessError | DataError | LoginErrorInvalidAuth | NotResearch deriving (Eq, Ord, Read, Show, Generic)


errorString :: Error ->  String
errorString err 
        | err == AccessError              =  "AccessError"
        | err == DataError            =  "DataError"
        | err == LoginErrorInvalidAuth              =  "LoginErrorInvalidAuth"
        | err == NotResearch =  "NotResearch"
        -- | err == CannotGetConfig =  "CannotGetConfig"
        -- | err == CannotGetMessage =  "CannotGetMessage"
        -- | err == CannotSendMessage =  "CannotSendMessage"
        -- | err == CantConvertFromData =  "CantConvertFromData"
