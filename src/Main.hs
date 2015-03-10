{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import           Servant

import           Model.Item (Item, Slug)
import           Model.User (AccessToken, WithRole)


type ItemApi = WithRole
                :> ( "items" :> Get [Item]                              -- GET /items
                :<|> "items" :> Capture "slug" Slug :> Get Item         -- GET /items/%slug
                :<|> "items" :> ReqBody Item :> Post Item               -- POST /items
                :<|> "backlinks" :> Capture "slug" Slug :> Get [Slug]   -- GET /backlinks/%slug
                :<|> "auth" :> Post AccessToken                         -- POST /auth
                )

main :: IO ()
main = return ()
