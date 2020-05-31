module Domain.Service.CommonService where

import ClassyPrelude
import qualified Domain.ImportEntity as E
import Data.Time
import Control.Monad.Except
import Katip

data Category = CatCategory1 E.Category1 | CatCategory2 E.Category2 | CatCategory3 E.Category3
data Entity   = 
    EntAuthor   E.Author   | 
    EntCategory Category   | 
    EntComment  E.Comment  | 
    EntDraft    E.Draft    |
    EntNews     E.News     | 
    EntUsers    E.Users    | 
    EntTeg      E.Teg
   -- -- Category | E.Author | E.Comment | E.Draft | E.News | E.Users | E.Teg

-- data family Entity a
-- data instance Entity Author = WrapAuthor Author

-- returnEntity :: Entity -> Entity
-- returnEntity (Nonempt (E.Author xx dd rr)) = Nonempt (E.Author (xx + 1) dd (rr + 1))


class Monad m =>  CommonService m  where
    create  :: Maybe Entity  -> m (Either E.Error Entity )
    editing :: Int -> m (Either E.Error Entity)
    getAll  :: m (Either E.Error [Entity])
    getOne  :: Int -> m (Either E.Error  Entity)
    remove  :: Int -> m (Either E.Error ())

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