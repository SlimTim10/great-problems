{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , module Data.Char
  , module Data.Either
  , module Control.Monad
  , module Control.Monad.Fix
  , module Control.Monad.Loops
  , module Control.Lens
  , module Text.Read
  , module Obelisk.Route
  , module Reflex.Dom.Core
  , headMay
  , showText
  , maybeToEither
  , singleton
  , safeToEnum
  , textToMaybe
  ) where

import Data.Text (Text)
import Data.Map (Map)
import Data.List (intercalate, intersperse, find, zip6)
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes, fromJust)
import Data.String.Conversions (cs)
import Data.CaseInsensitive (CI)
import Data.Functor ((<&>))
import Data.Word (Word64)
import Data.Char (ord)
import Data.Either (fromLeft, fromRight)
import Control.Monad (void, (<=<), forM, forM_, when, unless, (>=>), mfilter, guard)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Loops (andM)
import Control.Lens ((^.), _1, (&), (.~), iforM_)
import Text.Read (readMaybe)
import Obelisk.Route ( pattern (:/) )
import Reflex.Dom.Core ((&), (.~), (=:))

import qualified Data.Text as T

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

showText :: Show s => s -> Text
showText = T.pack . show

maybeToEither :: (a -> b) -> Maybe a -> Either b ()
maybeToEither f = maybe (pure ()) (Left . f)

singleton :: a -> [a]
singleton a = [a]

safeToEnum :: (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i =
  let r = toEnum i
      max' = maxBound `asTypeOf` r
      min' = minBound `asTypeOf` r
  in if i >= fromEnum min' && i <= fromEnum max'
     then Just r
     else Nothing

textToMaybe :: Text -> Maybe Text
textToMaybe "" = Nothing
textToMaybe x = Just x
