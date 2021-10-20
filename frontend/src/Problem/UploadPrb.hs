module Problem.UploadPrb
  ( widget
  ) where

import Frontend.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified GHCJS.DOM.Types
import qualified GHCJS.DOM.FileReader as FileReader
import qualified GHCJS.DOM.EventM as EventM
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

widget
  :: forall t m.
     ( R.DomBuilder t m
     , JS.MonadJSM m
     , R.TriggerEvent t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => m (R.Event t Text)
widget = do
  fi <- R.elClass
    "label"
    "cursor-pointer bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm min-w-fit active:bg-blue-400"
    $ do
    R.text "Upload PRB"
    fi1 <- R.inputElement $ R.def
      & R.initialAttributes .~ (
      "type" =: "file"
      <> "accept" =: ".prb"
      <> "class" =: "hidden"
      )
    return fi1
  readFileContents
    . R.fmapMaybe id
    . R.updated
    $ Util.headMay <$> R._inputElement_files fi

readFileContents
  :: forall t m.
     ( JS.MonadJSM m
     , R.TriggerEvent t m
     , R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Event t GHCJS.DOM.Types.File
  -> m (R.Event t Text)
readFileContents file = do
  fr <- JS.liftJSM FileReader.newFileReader
  R.performEvent_ (fmap (\f -> FileReader.readAsText fr (Just f) (Just "utf8" :: Maybe Text)) file)
  e :: R.Event t (Maybe Text) <- R.wrapDomEvent fr (`EventM.on` FileReader.load) . JS.liftJSM $ do
    v <- FileReader.getResult fr
    (JS.fromJSVal <=< JS.toJSVal) v
  return $ R.fmapMaybe id e
