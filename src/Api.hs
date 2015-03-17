{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Api(api) where

import           Control.Monad      (join)
import           Data.Text          (unpack)
import           Data.UUID          (fromString)
import           Network.HTTP.Types (QueryText, parseQueryText)
import           Network.Wai        (Application, rawQueryString)
import           Servant
import           Text.Read          (readMaybe)

import           Api.Definition     (ItemApi, WithRole, itemApi)
import           Model.Item         (backlinks, getItem, getItems, postItem)
import           Model.User         (AuthData (AuthData), Role (..), auth,
                                     tokenRole)

idTokenFromQuery :: QueryText -> Maybe AuthData
idTokenFromQuery qt = do uid <- join $ lookup "user_id" qt
                         at  <- join $ lookup "access_token" qt
                         user_id      <- readMaybe $ unpack uid
                         access_token <- fromString $ unpack at
                         return $ AuthData access_token user_id

instance HasServer a => HasServer (WithRole :> a) where
    type Server (WithRole :> a) = Role -> Server a
    route Proxy subserver request respond = do
        let querytext = parseQueryText $ rawQueryString request
        role <- case idTokenFromQuery querytext of
                    Nothing      -> return Guest
                    Just authdata -> tokenRole authdata
        route (Proxy :: Proxy a) (subserver role) request respond

server :: Server ItemApi
server = getItems
        :<|> getItem
        :<|> postItem
        :<|> backlinks
        :<|> auth

api :: Application
api = serve itemApi server
