module Domain.Types.Errors where

import Domain.Types.Imports
import ClassyPrelude

data Error = AccessError | DataError | LoginErrorInvalidAuth | LoginErrorEmailNotVerified