{-# LANGUAGE DeriveGeneric #-}
module Model.Item where

import           Control.Applicative                ((<$>), (<*>))
import           Control.Monad                      (liftM)
import           Data.Aeson                         (FromJSON, ToJSON, Value)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (PGArray (PGArray),
                                                     fromPGArray)
import           GHC.Generics                       (Generic)

data Visibility = Public | Family | Private
    deriving (Eq, Generic, Read, Show)

instance FromJSON Visibility
instance ToJSON Visibility

type Slug = Text

data Item = Item
    { created_at :: UTCTime
    , idata      :: Value -- TODO Object?
    , slug       :: Slug
    , tags       :: [Text]
    , title      :: Text
    , itype      :: Text
    , visibility :: Visibility
    } deriving Generic

instance FromJSON Item
instance ToJSON Item

instance FromRow Item where
    fromRow = Item <$> field <*> field <*> field <*> liftM fromPGArray field
                               <*> field <*> field <*> liftM read field

instance ToRow Item where
    toRow item = [ toField (created_at item)
                   , toField (idata item)
                   , toField (slug item)
                   , toField (PGArray $ tags item)
                   , toField (title item)
                   , toField (itype item)
                   , toField (show $ visibility item)
                   ]
