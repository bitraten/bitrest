{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Api(api) where

import           Network.Wai (Application)
import           Servant

import           Model.Item  (Item, Slug, backlinks, getItem, getItems,
                              postItem)
import           Model.User  (AuthResponse, User, WithRole, auth)

type ItemApi =  WithRole :> "items" :> Get [Item]                                   -- GET /items
                :<|> WithRole :> "items" :> Capture "slug" Slug :> Get Item         -- GET /items/%slug
                :<|> WithRole :> "items" :> ReqBody Item :> Post Item               -- POST /items
                :<|> WithRole :> "backlinks" :> Capture "slug" Slug :> Get [Slug]   -- GET /backlinks/%slug
                :<|> "auth" :> ReqBody User :> Post AuthResponse                 -- POST /auth

server :: Server ItemApi
server = getItems
        :<|> getItem
        :<|> postItem
        :<|> backlinks
        :<|> auth

itemApi :: Proxy ItemApi
itemApi = Proxy

api :: Application
api = serve itemApi server
