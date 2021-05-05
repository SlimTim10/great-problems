-- | A local Prelude, meant to be imported unqualified.
module Global
  ( Text
  , Map
  , module Control.Monad
  , module Control.Lens
  , module Reflex.Dom.Core
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Control.Monad (void, (<=<))
import Control.Lens ((^.))

import Reflex.Dom.Core ((&), (.~), (=:))
