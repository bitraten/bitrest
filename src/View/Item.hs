module View.Item where

import           Lucid

import           Model.Item.Definition (Item, title)
import           View.Layout           (layout)

sidebar :: Monad m => Item -> HtmlT m ()
sidebar item = toHtml "sidebar"

instance ToHtml Item where
  toHtml    item = layout    (toHtml $ title item) (toHtml $ title item) $ sidebar item
  toHtmlRaw item = toHtmlRaw $ title item

instance ToHtml [Item] where
  toHtml    items = layout    (toHtml $ title item) (toHtml $ title item) $ sidebar item
  toHtmlRaw item = toHtmlRaw $ title item
