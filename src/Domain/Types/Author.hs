module Domain.Types.Author where

import Domain.Types.Imports
import ClassyPrelude


data Author = Author {
    id_authors              :: Int,
    user_id_authors         :: Int,
    description_authors :: String
    } deriving (Show)

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field
