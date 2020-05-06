module Adapter.HTTP.Main where

import ClassyPrelude hiding (delete)
import Web.Scotty.Trans

mainH :: IO ()
mainH =
  scottyT 3000 id routes
routes :: (MonadIO m) => ScottyT LText m ()
routes =
  get "/hello" $ text "Hello!"


-- routes :: (MonadIO m) => ScottyT LText m ()
-- routes = do
--     get "/" $ text "home"
--     get "/hello/:name" $ do
--       name <- param ":name"
--       text $ "Hello, " <> name
--     post "/users" $ text "adding user"
--     put "/users/:id" $ text "updating user"
--     patch "/users/:id" $ text "partially updating users"
--     delete "/users/:id" $ text "deleting user"
--     matchAny "/admin" $ text "I don't care about your HTTP verb"
--     options (regex ".*") $ text "CORS usually use this"
--     notFound $ text "404"
--  sdf