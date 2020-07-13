module Domain.Service.CommonService where

import ClassyPrelude
import qualified Domain.ImportEntity as E
import Data.Time
import Control.Monad.Except


import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField
import GHC.Generics 
import Data.Aeson 
import Data.Time 



class Monad m =>  CommonService m  where
    create  :: E.Entity  -> m (Either E.Error Int64)
    editing :: E.Entity -> m (Either E.Error Int64)
    getAll  :: Text -> m (Either E.Error [E.Entity])
    getOne  :: Text -> Int ->  m (Either E.Error  E.Entity)
    remove  :: Text -> Int ->  m (Either E.Error Int64)

-- class Monad m =>  CommonService m  where
--     create  :: Bool -> Maybe (Entity a) -> m (Either Error (Entity a))
--     editing :: Bool -> (Entity a) -> m (Either Error (Entity a))
--     getAll  :: Bool -> String -> m (Either Error [(Entity a)])
--     getOne  :: Bool -> String -> Int -> m (Either Error  (Entity a))
--     remove  :: Bool -> (Entity a) -> m (Either Error ())


class Monad m =>  CategoryService m where
    allNestedCategory  :: E.Category ->  m [E.Category]
    allUpCategory      :: E.Category -> m [E.Category]
    allCategory        :: E.Category -> m [E.Category]
    

-- class CategoryForForall a where

-- instance CategoryForForall Category1 where
-- instance CategoryForForall Category2 where
-- instance CategoryForForall Category3 where
              

-- class EntityForForall a where
  
-- instance EntityForForall Category where
-- instance EntityForForall Author where
-- instance EntityForForall Comment where
-- instance EntityForForall Draft where
-- instance EntityForForall News where
-- instance EntityForForall Teg where
-- instance EntityForForall Users where



-- data Category = forall a. CategoryForForall  =>  Category a
-- data Entity   = forall a. EntityForForall  =>  Entity a

-- data family Family a