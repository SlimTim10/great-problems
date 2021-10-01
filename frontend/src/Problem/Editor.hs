module Problem.Editor
  ( widget
  ) where

import qualified Language.Javascript.JSaddle.Types as JS
import qualified Reflex.Dom.Ace as Ace
import qualified Reflex.Dom.Core as R

import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , R.TriggerEvent t m
     , JS.MonadJSM (R.Performable m)
     , JS.MonadJSM m
     , R.PerformEvent t m
     , R.PostBuild t m
     , R.MonadHold t m
     )
  => R.Event t Text -- ^ Used to force the content from an external source
  -> m (R.Dynamic t Text)
widget forcedValue = mdo
  let containerId = "editor"
  void $ R.elAttr "div" (
    "id" =: containerId
    <> "class" =: "h-full w-full border"
    ) R.blank
  (script, _) <- R.elAttr' "script" (
    "src" =: "/static/ace/ace.js"
    <> "type" =: "text/javascript"
    <> "charset" =: "utf-8"
    ) R.blank
  let scriptLoaded = () <$ R.domEvent R.Load script

  -- Make sure the forced value is set after Ace is loaded, in case the forced value event fires first
  afterScriptLoaded :: R.Event t () <- R.delay 0.001 scriptLoaded
  holdForcedValue :: R.Dynamic t Text <- R.holdDyn "" forcedValue
  let forcedValue' = R.leftmost [R.tagPromptlyDyn holdForcedValue afterScriptLoaded, forcedValue]
  
  let loading = R.el "p" $ R.text "Loading editor..." <&> const (R.constDyn "")
  dt :: R.Dynamic t (R.Dynamic t Text) <- R.widgetHold loading
    $ R.ffor scriptLoaded
    $ const $ do
      ace <- do
        let
          cfg = R.def
            { Ace._aceConfigMode = Just "latex"
            }
        Ace.aceWidget
          cfg
          (Ace.AceDynConfig (Just Ace.AceTheme_Clouds))
          R.never
          containerId
          ""
          forcedValue'
      return $ Ace.aceValue ace
  R.holdDyn "" . R.switchDyn $ R.updated <$> dt
