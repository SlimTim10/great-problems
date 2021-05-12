-- | A local Prelude, meant to be imported unqualified.
module Global
  ( Text
  , Map
  , module Data.Traversable
  , module Control.Monad
  , module Control.Lens
  , MonadFix
  , module Reflex.Dom.Core
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Traversable (for)
import Control.Monad (void, (<=<), forM_)
import Control.Lens ((^.), iforM_)
import Control.Monad.Fix (MonadFix)

import Reflex.Dom.Core ((&), (.~), (=:))
