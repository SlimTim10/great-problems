{-# LANGUAGE PatternSynonyms #-}

-- | A local Prelude, meant to be imported unqualified.
module Global
  ( Text
  , Map
  , module Data.Maybe
  , module Data.Bool
  , module Data.Traversable
  , module Data.Functor
  , module Control.Monad
  , module Control.Lens
  , MonadFix
  , module Data.String.Conversions
  , module Obelisk.Route
  , module Reflex.Dom.Core
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.Traversable (for)
import Data.Functor ((<&>))
import Control.Monad (void, (<=<), forM_)
import Control.Lens ((^.), iforM_, _1)
import Control.Monad.Fix (MonadFix)
import Data.String.Conversions (cs)

import Obelisk.Route ( pattern (:/) )
import Reflex.Dom.Core ((&), (.~), (=:))
