module View.Item where

import           Lucid.Base            (ToHtml, toHtml, toHtmlRaw)

import           Model.Item.Definition (Item, title)

instance ToHtml Item where
  toHtml    item = toHtml    $ title item
  toHtmlRaw item = toHtmlRaw $ title item
