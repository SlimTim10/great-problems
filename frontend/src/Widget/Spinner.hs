module Widget.Spinner
  ( small
  , holdSmall
  ) where

import Common.Lib.Prelude

import qualified Reflex.Dom.Core as R
import qualified Obelisk.Generated.Static as Ob

holdSmall
  :: forall t m a.
     ( R.DomBuilder t m
     , R.MonadHold t m
     )
  => R.Event t a
  -> m (R.Dynamic t (m ()))
holdSmall e = R.holdDyn R.blank $ R.ffor e . const $ small

small :: R.DomBuilder t m => m ()
small = do
  R.elAttr
    "img"
    ("src" =: Ob.static @"small_spinner.svg" <> "width" =: "30" <> "alt" =: "loading")
    $ do
    R.blank
