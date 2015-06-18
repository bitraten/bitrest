{-# LANGUAGE OverloadedStrings #-}

module Model.User(AuthData(..), Role (..), User, auth, tokenRole) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT, left)
import           Database.PostgreSQL.Simple (Only (Only), fromOnly)
import           Servant                    (ServantErr, err401)

import           Database                   (query)
import           Model.User.Definition      (AuthData (..), User (..))

data Role = Guest | Member | Admin
    deriving Read

genToken :: Int -> IO AuthData
genToken userid = do token <- query
                        "INSERT INTO tokens (access_token, user_id) VALUES (uuid_generate_v4(), ?) RETURNING access_token"
                        $ Only userid
                     return $ AuthData (head $ map fromOnly token) userid


tokenRole :: AuthData -> IO Role
tokenRole authdata = do role <- query
                            "SELECT role FROM users, tokens where id = user_id AND user_id = ? AND access_token = ?"
                            (user_id authdata, access_token authdata)
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

auth :: User -> EitherT ServantErr IO AuthData
auth user = do mID <- liftIO $ userID user
               case mID of
                    Nothing  -> left err401
                    Just uid -> liftIO $ genToken uid
