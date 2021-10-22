{-# LANGUAGE PatternSynonyms #-}
-- | A local Prelude, meant to be imported unqualified.
module Common.Lib.Prelude
  ( module Data.Text
  , module Data.Map
  , module Data.List
  , module Data.Maybe
  , module Data.String.Conversions
  , module Data.CaseInsensitive
  , module Data.Functor
  , module Data.Word
  , module Data.Either
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Lens
  , module Text.Read
  , module Obelisk.Route
  , module Reflex.Dom.Core
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, fromJust)
import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI)
import Data.Functor ((<&>))
import Data.Word (Word64)
import Data.Either (fromLeft)
import Control.Monad (void, (<=<), forM_, when, unless, (>=>), mfilter)
import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.), _1, (&), (.~), iforM_)
import Text.Read (readMaybe)
import Obelisk.Route ( pattern (:/) )
import Reflex.Dom.Core ((&), (.~), (=:))