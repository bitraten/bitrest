{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Definition(ItemApi, WithRole, itemApi) where

import           Servant
import           Servant.HTML.Lucid (HTML)

import           Model.Item.Definition (Item, Slug)
import           Model.User.Definition (AuthData, User, WithRole)

type ItemApi =  WithRole :> "items" :> Get '[JSON] [Item]                                   -- GET /items
                :<|> WithRole :> "items" :> Capture "slug" Slug :> Get '[JSON, HTML] Item   -- GET /items/%slug
                :<|> WithRole :> "items" :> ReqBody '[JSON] Item :> Post '[JSON] Item       -- POST /items
                :<|> WithRole :> "backlinks" :> Capture "slug" Slug :> Get '[JSON] [Slug]   -- GET /backlinks/%slug
                :<|> "auth" :> ReqBody '[JSON] User :> Post '[JSON] AuthData                -- POST /auth

itemApi :: Proxy ItemApi
itemApi = Proxy
