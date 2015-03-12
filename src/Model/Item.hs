{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Item(Item, Slug, backlinks, getItem, getItems, postItem) where

import           Control.Applicative                ((<$>), (<*>))
import           Control.Error                      (EitherT, left, tryHead)
import           Control.Monad                      (liftM)
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson                         (FromJSON, ToJSON, Value)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import           Database.PostgreSQL.Simple         (In (In), Only (Only),
                                                     fromOnly)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField, toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (PGArray (PGArray),
                                                     fromPGArray)
import           GHC.Generics                       (Generic)

import           Database                           (execute, query)
import           Model.User                         (Role (Guest, Member, Admin))

data Visibility = Public | Family | Private
    deriving (Eq, Generic, Read, Show)

instance FromJSON Visibility
instance ToJSON Visibility

instance ToField Visibility where
    toField = toField . show

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
                   , toField (visibility item)
                   ]

roleVis :: Role -> [Visibility]
roleVis Guest  = [Public]
roleVis Member = [Public, Family]
roleVis Admin  = [Public, Family, Private]

getItems :: Role -> EitherT (Int, String) IO [Item]
getItems role = liftIO $ query
                    "SELECT * FROM items WHERE visibility IN ?"
                    (Only (In $ roleVis role))

getItem :: Role -> Slug -> EitherT (Int, String) IO Item
getItem role slug = do items <- liftIO $ query
                            "SELECT * FROM items WHERE visibility IN ? AND slug = ?"
                            (In $ roleVis role, slug)
                       tryHead (404, "not found") items

postItem :: Role -> Item -> EitherT (Int, String) IO Item
postItem Admin item = liftIO $ execute
                        "INSERT INTO items VALUES (?, ?, ?, ? :: text[], ?, ?, ? )"
                        item >> return item
postItem _     _    = left (401, "unauthorized")

backlinks :: Role -> Slug -> EitherT (Int, String) IO [Slug]
backlinks role slug = do slugs <- liftIO $ query
                            "SELECT slug FROM items WHERE visibility IN ? AND ? = ANY(tags)"
                            (In $ roleVis role, slug)
                         return $ map fromOnly slugs
