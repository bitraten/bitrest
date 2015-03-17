{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Model.User(AuthResponse, Role (..), User, WithRole, auth) where

import           Control.Error              (EitherT, left)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (unpack)
import           Data.UUID                  (UUID, fromString)
import           Database.PostgreSQL.Simple (Only (Only), fromOnly)
import           Network.HTTP.Types         (QueryText, parseQueryText)
import           Network.Wai                (rawQueryString)
import           Servant
import           Text.Read                  (readMaybe)

import           Database                   (query)
import           Model.User.Definition      (AuthResponse (..), User (..),
                                             WithRole)
data Role = Guest | Member | Admin
    deriving Read

instance HasServer a => HasServer (WithRole :> a) where
    type Server (WithRole :> a) = Role -> Server a
    route Proxy subserver request respond = do
        let querytext = parseQueryText $ rawQueryString request
        role <- case idTokenFromQuery querytext of
                    Nothing      -> return Guest
                    Just idtoken -> uncurry tokenRole idtoken
        route (Proxy :: Proxy a) (subserver role) request respond

idTokenFromQuery :: QueryText -> Maybe (Int, UUID)
idTokenFromQuery qt = do uid <- join $ lookup "user_id" qt
                         at  <- join $ lookup "access_token" qt
                         user_id      <- readMaybe $ unpack uid
                         access_token <- fromString $ unpack at
                         return (user_id, access_token)

genToken :: Int -> IO AuthResponse
genToken userid = do token <- query
                        "INSERT INTO tokens (access_token, user_id) VALUES (uuid_generate_v4(), ?) RETURNING access_token"
                        $ Only userid
                     return $ AuthResponse (head $ map fromOnly token) userid


tokenRole :: Int -> UUID -> IO Role
tokenRole userid token = do role <- query
                                "SELECT role FROM users, tokens where id = user_id AND user_id = ? AND access_token = ?"
                                (userid, token)
                            return $ case role of
                                        [] -> Guest
                                        _  -> head $ map (read . fromOnly) role

userID :: User -> IO (Maybe Int)
userID user = do uids <- query
                    "SELECT id FROM users WHERE username = ? AND password = CRYPT(?, password)"
                    (username user, password user)
                 return $ case uids of
                            [] -> Nothing
                            _  -> head $ map fromOnly uids

auth :: User -> EitherT (Int, String) IO AuthResponse
auth user = do mID <- liftIO $ userID user
               case mID of
                    Nothing  -> left (401, "unauthorized")
                    Just uid -> liftIO $ genToken uid
