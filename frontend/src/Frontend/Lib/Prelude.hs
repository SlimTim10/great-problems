{-# LANGUAGE PatternSynonyms #-}

-- | A local Prelude, meant to be imported unqualified.
module Frontend.Lib.Prelude
  ( module Data.Text
  , module Data.Map
  , module Data.Maybe
  , module Data.String.Conversions
  , module Data.Functor
  , module Data.Word
  , module Data.Either
  , module Control.Monad.Fix
  , module Control.Monad
  , module Control.Lens
  , module Obelisk.Route
  , module Reflex.Dom.Core
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, fromJust)
import Data.String.Conversions (cs)
import Data.Functor ((<&>))
import Data.Word (Word64)
import Data.Either (fromLeft)
import Control.Monad (void, (<=<), forM_, when, unless, (>=>), mfilter)
import Control.Monad.Fix (MonadFix)
import Control.Lens ((^.), iforM_, _1)

import Obelisk.Route ( pattern (:/) )
import Reflex.Dom.Core ((&), (.~), (=:))
