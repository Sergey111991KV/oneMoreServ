module Adapter.HTTP.Auth where

import ClassyPrelude
import Web.Scotty
import Adapter.HTTP.Common


show :: User -> MyAction s ()
show user = do