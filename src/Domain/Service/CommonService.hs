module Domain.Service.CommonService where

import ClassyPrelude
import Domain.ImportEntity
import Data.Time
import Control.Monad.Except
import Katip


class CommonService a where
    create  :: Users -> Maybe m -> Either Error m
    editing :: Users -> m -> Either Error m
    getAll  :: Users -> Either Error [m]
    getOne  :: Users ->  Int -> Either Error  m
    remove  :: Users ->  m -> Either Error ()


class  CategoryService a where
    allNestedCategory  :: a -> [a]
    allUpCategory      :: a -> [a]
    allCategory        :: a -> [a]
    
-- instance CategoryService Category1 where
   

-- instance CategoryService Category2 where

    
-- instance CategoryService Category3 where
   

instance CategoryService Category where


-- instance CommonService Author where

-- instance CommonService Category where

-- instance CommonService Comment where

-- instance CommonService Draft where
                    
-- instance CommonService News where
                    
-- instance CommonService Teg where
                    
-- instance CommonService Users where

instance CommonService Entity where


data Category = forall a. CategoryService a =>  Category a
data Entity   = forall a. CommonService a =>  Entity a