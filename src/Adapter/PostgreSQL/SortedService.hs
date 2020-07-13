module Adapter.PostgreSQL.SortedService where

import ClassyPrelude
import Data.Time
import Control.Monad.Except

import Database.PostgreSQL.Simple
import Data.Maybe
import Data.Text.Time
import Data.Attoparsec.Text
import Data.Text

import qualified Domain.ImportService as S
import qualified Domain.ImportEntity as E
import Adapter.PostgreSQL.CommonPostgres as CP