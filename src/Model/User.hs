{-# LANGUAGE OverloadedStrings #-}

module Model.User(AuthResponse, Role (..), User, auth, tokenRole) where

import           Control.Error              (EitherT, left)
import           Control.Monad.IO.Class     (liftIO)
import           Data.UUID                  (UUID)
import           Database.PostgreSQL.Simple (Only (Only), fromOnly)

import           Database                   (query)
import           Model.User.Definition      (AuthResponse (..), User (..))

data Role = Guest | Member | Admin
    deriving Read

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
