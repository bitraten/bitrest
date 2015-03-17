module Api(api) where

import           Network.Wai    (Application)
import           Servant

import           Api.Definition (ItemApi, itemApi)
import           Model.Item     (backlinks, getItem, getItems, postItem)
import           Model.User     (auth)

server :: Server ItemApi
server = getItems
        :<|> getItem
        :<|> postItem
        :<|> backlinks
        :<|> auth

api :: Application
api = serve itemApi server
