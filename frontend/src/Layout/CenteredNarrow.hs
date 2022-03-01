module Layout.CenteredNarrow
  ( widget
  ) where

import qualified Reflex.Dom.Core as R

widget
  :: forall t m.
     ( R.DomBuilder t m
     )
  => m ()
  -> m ()
widget x = do
  R.elClass "div" "mt-10 flex justify-center" $ do
    R.elClass "div" "flex flex-col gap-4 w-80" $ do
      x
