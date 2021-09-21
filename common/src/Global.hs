{-# LANGUAGE PatternSynonyms #-}

-- | A local Prelude, meant to be imported unqualified.
module Global
  ( Text
  , Map
  , module Data.Maybe
  , module Data.Bool
  , module Data.Traversable
  , module Data.Functor
  , module Data.List
  , module Data.Either
  , module Data.Word
  , module Control.Monad
  , module Control.Lens
  , MonadFix
  , module Data.String.Conversions
  , CI
  , module Text.Read
  , module Obelisk.Route
  , module Reflex.Dom.Core
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.Bool (bool)
import Data.Traversable (for)
import Data.Functor ((<&>))
import Data.List (intercalate, intersperse)
import Data.Either (fromLeft)
import Data.Word (Word64)
import Control.Monad (void, (<=<), forM_, when, unless, (>=>))
import Control.Lens ((^.), iforM_, _1)
import Control.Monad.Fix (MonadFix)
import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI)
import Text.Read (readMaybe)

import Obelisk.Route ( pattern (:/) )
import Reflex.Dom.Core ((&), (.~), (=:))
