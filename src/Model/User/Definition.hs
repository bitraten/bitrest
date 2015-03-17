{-# LANGUAGE DeriveGeneric #-}

module Model.User.Definition(AuthResponse(..), User(..), WithRole) where

import           Data.Aeson   (FromJSON, ToJSON, toJSON)
import           Data.Text    (Text)
import           Data.UUID    (UUID, toString)
import           GHC.Generics (Generic)


data AuthResponse = AuthResponse {access_token :: UUID, user_id :: Int}
    deriving Generic

data User = User {username :: Text, password :: Text}
    deriving (Generic)

instance ToJSON UUID
    where toJSON = toJSON . toString
instance ToJSON AuthResponse

instance FromJSON User

data WithRole
