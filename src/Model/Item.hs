{-# LANGUAGE OverloadedStrings #-}

module Model.Item(Item, Slug, backlinks, getItem, getItems, postItem) where

import           Control.Monad                      (liftM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Either         (EitherT, left)
import           Database.PostgreSQL.Simple         (In (In), Only (Only),
                                                     fromOnly)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField (ToField, toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (PGArray (PGArray),
                                                     fromPGArray)
import           Servant                            (ServantErr, err401, err404)

import           Database                           (execute, query)
import           Model.Item.Definition              (Item (..), Slug,
                                                     Visibility (..))
import           Model.User                         (Role (..))

instance ToField Visibility where
    toField = toField . show

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

getItems :: Role -> EitherT ServantErr IO [Item]
getItems role = liftIO $ query
                    "SELECT * FROM items WHERE visibility IN ?"
                    (Only (In $ roleVis role))

getItem :: Role -> Slug -> EitherT ServantErr IO Item
getItem role slug = do items <- liftIO $ query
                            "SELECT * FROM items WHERE visibility IN ? AND slug = ?"
                            (In $ roleVis role, slug)
                       tryHead err404 items
                    where tryHead e []     = left e
                          tryHead _ (i:_) = return i

postItem :: Role -> Item -> EitherT ServantErr IO Item
postItem Admin item = liftIO $ execute
                        "INSERT INTO items (created_at, idata, slug, tags, title, itype, visibility) VALUES (?, ?, ?, ? :: text[], ?, ?, ? )"
                        item >> return item
postItem _     _    = left err401

backlinks :: Role -> Slug -> EitherT ServantErr IO [Slug]
backlinks role slug = do slugs <- liftIO $ query
                            "SELECT slug FROM items WHERE visibility IN ? AND ? = ANY(tags)"
                            (In $ roleVis role, slug)
                         return $ map fromOnly slugs
