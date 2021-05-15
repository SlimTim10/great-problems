{-# LANGUAGE TypeFamilies #-}

module Problem.Editor
  ( widget
  ) where

import qualified Ace

import qualified Language.Javascript.JSaddle.Types as JS
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
  => R.Event t Text
  -> m (R.Dynamic t Text)
widget forcedValue = do
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
  let loading = R.el "p" $ R.text "Loading editor..." <&> const (R.constDyn "")
  dt :: R.Dynamic t (R.Dynamic t Text) <- R.widgetHold loading $ R.ffor scriptLoaded $ const $ do
    ace <- do
      let
        cfg = R.def
          { Ace._aceConfigMode = Just "latex"
          }
      Ace.aceWidget cfg (Ace.AceDynConfig (Just Ace.AceTheme_Clouds)) R.never containerId ""
    return $ Ace.aceValue ace
  R.holdDyn "" . R.switchDyn $ R.updated <$> dt

  ------------------------------------------------------------------------
  -- t <- R.textAreaElement $ R.def
  --   & R.setValue .~ forcedValue
  --   & R.initialAttributes .~ ("class" =: "h-full w-full border")
  -- return $ R.value t
