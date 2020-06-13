module Domain.Types.Entity where

import Domain.Types.Imports
import ClassyPrelude
import Database.PostgreSQL.Simple.FromField 
import Domain.Types.AccesTypes 
import Domain.Types.Author     
import Domain.Types.Category   
import Domain.Types.Comment    
import Domain.Types.Draft      
import Domain.Types.News       
import Domain.Types.Teg        
import Domain.Types.Errors     


data Category = CatCategory1 Category1 | CatCategory2 Category2 | CatCategory3 Category3 deriving (Show, Eq, Generic)

instance FromJSON Category
instance ToJSON Category
-- instance  ToRow Category


data Entity   = 
    EntAuthor   Author   | 
    EntCategory Category   | 
    EntComment  Comment  | 
    EntDraft    Draft    |
    EntNews     News     | 
    EntUsers    Users    | 
    EntTeg      Teg deriving (Show, Eq, Generic) 


                                        -- Convert throw data


class ConvertEntity a where
            convertToEntity :: a -> Entity
            convertFromEntity :: Entity -> a

instance ConvertEntity Author where
            convertToEntity a =  EntAuthor a
            convertFromEntity (EntAuthor a) = a

instance ConvertEntity Category where
            convertToEntity a =  EntCategory a
            convertFromEntity (EntCategory a) = a

instance ConvertEntity Comment where
            convertToEntity a =  EntComment a
            convertFromEntity (EntComment a) = a

instance ConvertEntity Draft where
            convertToEntity a =  EntDraft a
            convertFromEntity (EntDraft a) = a

instance ConvertEntity News where
            convertToEntity a =  EntNews a
            convertFromEntity (EntNews a) = a

instance ConvertEntity Users where
            convertToEntity a =  EntUsers a
            convertFromEntity (EntUsers a) = a

instance ConvertEntity Teg where
            convertToEntity a =  EntTeg a
            convertFromEntity (EntTeg a) = a


-- convertNewsToEntity ::   E.News ->  E.Entity
-- convertNewsToEntity (E.News q w e r t y u i) =  E.EntNews (E.News q w e r t y u i)
            
--             -- convertToEntity :: a ->  E.Entity
--             -- convertToEntity = undefined
            
--             -- convertFromEntity ::  E.Entity  -> a
--             -- convertFromEntity ent = 
--             --   case ent of 
--             --       E.EntNews (E.News q w e r t y u i) -> (E.News q w e r t y u i)
--             --       _ -> False
                 
            

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