module Adapter.PostgreSQL.SearchIn where

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

-- class CommonService m  => SearchIn m  where
--     inContent :: Text -> m (Either E.Error [E.News] )   -- API новостей должно поддерживать поиск по строке, которая может быть найдена либо в текстовом контенте,
--     inEntyty  :: Text -> m (Either E.Error [E.News] )    --  либо в имени автора, либо в названии категории/тега


inContent  :: (PG r m, S.CommonService m)  =>  Text -> m (Either E.Error [E.News])
-- inContent = undefined

inContent text = do
    all <- S.getAll "news"
    case all of 
        Left err -> return $ Left err
        Right allEntyNews -> do
                let rightNews = compareTextInTextNews text (fmap E.convertFromEntity allEntyNews)
                return rightNews
                
                   

compareTextInTextNews :: Text -> [E.News] -> (Either E.Error [E.News])
compareTextInTextNews text news =  foldB (fmap (parseTextNews text) news)
-- compareTextInTextNews text news = fmap (parseTextNews text) news

foldB :: [Maybe E.News] -> (Either E.Error [E.News])
-- foldB []     = Left E.NotResearch
foldB x = if z == [] then Left E.NotResearch else Right z
    where 
        z = foldB' x 

foldB' :: [Maybe E.News] -> [E.News]
foldB' [] = []
foldB' (x:xs) = if x == Nothing then (foldB' xs) else [z] ++ (foldB' xs)
        where z = retIsMaybe x

retIsMaybe :: Maybe a ->  a
retIsMaybe (Just a) = a


parseTextNews :: Text -> E.News -> Maybe E.News
parseTextNews t1 news = if (Data.Text.count t1 t2) > 0 then (Just news) else Nothing
                where 
                    t2 = E.text_news news



-- split :: (Char -> Bool) -> Text -> [Text]

-- parseTextNews :: E.News -> [Text]
-- parseTextNews news = Data.Text.words t
--                 where 
--                     t = E.text_news news


-- textCount :: Text -> Text -> Int
-- textCount t t1 = Data.Text.count t t1


-- textCount1 = textCount "hello" "hello word"
-- textCount2 = textCount "hello" "hell word"
-- textCount3 = textCount "hello" "hello hello  hello word"
-- textCount22 = textCount "hello" "hellohellohello word"


-- compareText :: Text -> Text -> Bool
-- compareText t1 t2 = if t1 == t2 then True else False

-- compareNewsText :: Text -> Text -> Bool
-- compareNewsText t1 (x:xs) = if t1 == x then True else False


-- parseTextNews :: Text -> E.News -> Either Text Int
-- parseTextNews t1 news  = if (compareText t1 t2) then Right idN  else Left "No similar"
--                     where
--                         t2  = E.text_news news
--                         idN = E.id_news news



-- validFroTextNews :: Text -> [E.News] -> Either E.Error [E.News]
-- validFroTextNews text (x:xs) = parseInSearchText text (text_news x)

-- parse' :: Parsec Text u [Int]
-- parse' = undefined 

-- parseWordToWord :: Text -> Text -> Bool
-- parseWordToWord = undefined

-- parseTextToWord :: Text -> [Text]
-- parseTextToWord = undefined

-- coincidenceWord :: E.News -> Bool -> Int
-- coincidenceWord  = undefined

-- coincidenceAllNews :: Text -> [E.News] -> [Int]
-- coincidenceAllNews = undefined






        --  :l Adapter.PostgreSQL.SearchInService