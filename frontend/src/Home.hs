module Home
  ( widget
  ) where

import qualified Control.Monad.Fix as Fix
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import qualified Common.Api as Api
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     , JS.MonadJSM m
     , JS.MonadJSM (R.Performable m)
     , R.HasJSContext (R.Performable m)
     , R.PerformEvent t m
     , R.TriggerEvent t m
     )
  => m ()
widget = do
  R.el "div" $ do
    onload <- R.getPostBuild
    let endpoint :: R.Event t Text = R.tag (R.current (R.constDyn "/api/problems")) onload
    response :: R.Event t (Maybe [Api.Problem]) <- R.getAndDecode endpoint
    problems :: R.Dynamic t [Api.Problem] <- R.holdDyn [] $ fromMaybe [] <$> response
    void $ R.simpleList problems problemItem

problemItem
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     )
  =>  R.Dynamic t Api.Problem -> m ()
problemItem problem = R.elClass "div" "border" $ do
  R.elClass "p" "" $ do
    R.dynText $ "Title: " <> (Api.title <$> problem)
  void $ R.dyn $ R.ffor (Api.description <$> problem) $ \case
    Nothing -> R.blank
    Just description -> R.elClass "p" "" $ do
      R.text $ "Description: " <> description
  R.elDynAttr "img"
    (("src" =:) <$> Api.thumnail_url <$> problem)
    R.blank
