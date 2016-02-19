{-# LANGUAGE OverloadedStrings #-}
module View.Layout where

import           Lucid

left :: Monad m => HtmlT m ()
left = "Left"

layout :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m () -> HtmlT m ()
layout title body sidebar = doctypehtml_ $ do
                              head_ [] $
                                title_ [] title
                              body_ [] $
                                div_ [class_ "container"] $ do
                                  header_ [class_ "row"] $ do
                                    div_ [class_ "col c3"] " "
                                    h1_ [class_ "col c6"] "bitraten."
                                    div_ [class_ "col c3"] "Login buttons"
                                  div_ [class_ "row"] $ do
                                    aside_ [class_ "col c3 left"] left
                                    main_ [class_ "col c6"] body
                                    aside_ [class_ "col c3 right"] sidebar
