module Problem.Loading
  ( WithLoading(..)
  , anyLoading
  , latestAction
  ) where

import qualified Reflex.Dom.Core as R

data WithLoading a = WithLoading
  { action :: a
  , loading :: Bool
  }

anyLoading
  :: forall t m a.
     ( R.MonadHold t m
     , R.PerformEvent t m
     , R.MonadSample t (R.Performable m)
     )
  => [R.Dynamic t (WithLoading a)]
  -> m (R.Dynamic t Bool)
anyLoading xs = do
  let loadings :: [R.Event t Bool] = map (R.updated . fmap loading) $ xs
  anyLoading' :: R.Event t Bool <- R.performEvent $ R.ffor (R.leftmost loadings) $ \_ -> do
    loadingSamples <- mapM (R.sample . R.current) . map (fmap loading) $ xs
    return $ any (== True) loadingSamples
  R.holdDyn False anyLoading'

latestAction :: R.Reflex t => [R.Dynamic t (WithLoading a)] -> R.Event t a
latestAction = R.leftmost . map (R.updated . fmap action)
