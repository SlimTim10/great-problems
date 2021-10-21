{-# LANGUAGE PatternSynonyms #-}

-- | A local Prelude, meant to be imported unqualified.
module Backend.Lib.Prelude
  ( module Data.Text
  , module Data.Map
  , module Data.List
  , module Data.Maybe
  , module Data.CaseInsensitive
  , module Data.String.Conversions
  , module Data.Functor
  , module Data.Word
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Lens
  , module Text.Read
  , module Obelisk.Route
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, fromJust)
import Data.CaseInsensitive (CI)
import Data.String.Conversions (cs)
import Data.Functor ((<&>))
import Data.Word (Word64)
import Control.Monad (void, (<=<), forM_, when, unless, (>=>), mfilter)
import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.), _1, (&), (.~))
import Text.Read (readMaybe)

import Obelisk.Route ( pattern (:/) )
