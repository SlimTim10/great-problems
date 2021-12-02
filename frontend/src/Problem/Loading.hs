module Problem.Loading
  ( WithLoading(..)
  , anyLoading
  , latestAction
  ) where

import qualified Reflex.Dom.Core as R

data WithLoading a = WithLoading
  { action :: a
  , loading :: Bool
  } deriving (Show, Eq)

anyLoading
  :: forall t m a.
     ( R.MonadHold t m
     , R.PerformEvent t m
     )
  => [R.Dynamic t (WithLoading a)]
  -> m (R.Dynamic t Bool)
anyLoading = R.holdDyn False . R.mergeWith (&&) . map (R.updated . fmap loading)

latestAction
  :: R.Reflex t
  => [R.Dynamic t (WithLoading a)]
  -> R.Event t a
latestAction = R.leftmost . map (R.updated . fmap action)
