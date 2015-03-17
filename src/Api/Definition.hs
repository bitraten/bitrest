{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Definition(ItemApi, WithRole, itemApi) where

import           Servant

import           Model.Item.Definition (Item, Slug)
import           Model.User.Definition (AuthResponse, User, WithRole)

type ItemApi =  WithRole :> "items" :> Get [Item]                                   -- GET /items
                :<|> WithRole :> "items" :> Capture "slug" Slug :> Get Item         -- GET /items/%slug
                :<|> WithRole :> "items" :> ReqBody Item :> Post Item               -- POST /items
                :<|> WithRole :> "backlinks" :> Capture "slug" Slug :> Get [Slug]   -- GET /backlinks/%slug
                :<|> "auth" :> ReqBody User :> Post AuthResponse                 -- POST /auth

itemApi :: Proxy ItemApi
itemApi = Proxy
