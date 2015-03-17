{-# LANGUAGE DeriveGeneric #-}

module Model.Item.Definition (Item(..), Slug, Visibility(..)) where

import           Data.Aeson      (FromJSON, ToJSON, Value)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics    (Generic)

data Visibility = Public | Family | Private
    deriving (Eq, Generic, Read, Show)

instance FromJSON Visibility
instance ToJSON Visibility

type Slug = Text

data Item = Item
    { created_at :: UTCTime
    , idata      :: Value -- TODO Object?
    , slug       :: Slug
    , tags       :: [Slug]
    , title      :: Text
    , itype      :: Text
    , visibility :: Visibility
    } deriving Generic

instance FromJSON Item
instance ToJSON Item
