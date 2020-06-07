module Domain.Service.CommonService where

import ClassyPrelude
import qualified Domain.ImportEntity as E
import Data.Time
import Control.Monad.Except
import Katip

import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.ToRow 
import Database.PostgreSQL.Simple.FromRow 
import Database.PostgreSQL.Simple.ToField 
import Database.PostgreSQL.Simple.FromField
import GHC.Generics 
import Data.Aeson 
import Data.Time 

data Category = CatCategory1 E.Category1 | CatCategory2 E.Category2 | CatCategory3 E.Category3 deriving (Show, Eq, Generic)

instance FromJSON Category
instance ToJSON Category
-- instance  ToRow Category


data Entity   = 
    EntAuthor   E.Author   | 
    EntCategory Category   | 
    EntComment  E.Comment  | 
    EntDraft    E.Draft    |
    EntNews     E.News     | 
    EntUsers    E.Users    | 
    EntTeg      E.Teg deriving (Show, Eq, Generic) 

    
-- instance FromRow Entity where
--     fromRow  =  EntAuthor <$> field 

-- instance FromJSON Entity
-- instance ToJSON Entity
-- instance  ToRow Entity 


-- instance FromField Entity  
-- -- where
--         fromField f bs = do
--             case entity of
--                 (E.Users user) -> EntUsers (E.Users user)
--             where entity 
   -- -- Category | E.Author | E.Comment | E.Draft | E.News | E.Users | E.Teg

-- data family Entity a
-- data instance Entity Author = WrapAuthor Author

-- returnEntity :: Entity -> Entity
-- returnEntity (Nonempt (E.Author xx dd rr)) = Nonempt (E.Author (xx + 1) dd (rr + 1))


class Monad m =>  CommonService m  where
    create  :: Entity  -> m (Either E.Error Int64 )
    editing :: Entity -> m (Either E.Error Int64)
    getAll  :: Text -> m (Either E.Error [Entity])
    getOne  :: Int -> m (Either E.Error  Entity)
    remove  :: Int -> m (Either E.Error ())
    testAction  :: m (Either E.Error Int64 )

-- class Monad m =>  CommonService m  where
--     create  :: Bool -> Maybe (Entity a) -> m (Either Error (Entity a))
--     editing :: Bool -> (Entity a) -> m (Either Error (Entity a))
--     getAll  :: Bool -> String -> m (Either Error [(Entity a)])
--     getOne  :: Bool -> String -> Int -> m (Either Error  (Entity a))
--     remove  :: Bool -> (Entity a) -> m (Either Error ())


class Monad m =>  CategoryService m where
    allNestedCategory  :: Category ->  m [Category]
    allUpCategory      :: Category -> m [Category]
    allCategory        :: Category -> m [Category]
    

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