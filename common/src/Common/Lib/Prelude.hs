-- | A local Prelude, meant to be imported unqualified.
module Common.Lib.Prelude
  ( module Data.Text
  , module Data.Map
  , module Data.Maybe
  , module Data.String.Conversions
  , module Data.CaseInsensitive
  , module Text.Read
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI)
import Text.Read (readMaybe)
