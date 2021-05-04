{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Problem.Editor
  ( widget
  ) where

import qualified Reflex.Dom.Core as FRP

import Global

widget
  :: ( FRP.DomBuilder t m
     )
  => m (FRP.Dynamic t Text)
widget = FRP.elAttr "div" ("style" =: "border: 1px solid black;") $ do
  FRP.el "h2" $ FRP.text "Editor"
  t <- FRP.textAreaElement $ FRP.def
  return $ FRP.value t
