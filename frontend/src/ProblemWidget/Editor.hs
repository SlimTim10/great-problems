{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ProblemWidget.Editor
  ( editorWidget
  ) where

import Data.Text (Text)

import Reflex.Dom.Core

editorWidget
  :: ( DomBuilder t m
     )
  => m (Dynamic t Text)
editorWidget = elAttr "div" ("style" =: "border: 1px solid black;") $ do
  el "h2" $ text "Editor"
  t <- textAreaElement $ def
  return $ value t
