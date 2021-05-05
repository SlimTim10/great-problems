module Problem.UploadPrb
  ( widget
  ) where

import qualified JSDOM.File
import qualified JSDOM.Types
import qualified JSDOM.FileReader as FileReader
import qualified JSDOM.EventM as EventM
import qualified Language.Javascript.JSaddle as JS
import qualified Reflex.Dom.Core as R

import Util
import Global

widget
  :: forall t m.
     ( R.DomBuilder t m
     , JS.MonadJSM m
     , R.TriggerEvent t m
     , R.MonadSample t m
     , R.MonadHold t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => m (R.Event t Text)
widget = do
  fi <- R.el "label" $ do
    R.text "Upload PRB"
    fi1 <- R.inputElement $ R.def & R.initialAttributes .~ (
      "type" =: "file"
      <> "accept" =: ".prb"
      )
    return fi1
  readFileContents
    . R.fmapMaybe id
    . R.updated
    $ headMay <$> R._inputElement_files fi
  -- -- For more detailed reference
  -- let dfs :: R.Dynamic t [JSDOM.Types.File] = R._inputElement_files fi
  -- let dmf :: R.Dynamic t (Maybe JSDOM.Types.File) = headMay <$> dfs
  -- let ef :: R.Event t JSDOM.Types.File = R.fmapMaybe id (R.updated dmf)
  -- evFileContents :: R.Event t Text <- readFileContents ef
  -- return evFileContents

readFileContents
  :: forall t m.
     ( JS.MonadJSM m
     , R.TriggerEvent t m
     , R.DomBuilder t m
     , R.PerformEvent t m
     , JS.MonadJSM (R.Performable m)
     )
  => R.Event t JSDOM.Types.File
  -> m (R.Event t Text)
readFileContents ef = do
  fr <- JS.liftJSM FileReader.newFileReader
  R.performEvent_ (fmap (\f -> FileReader.readAsText fr (Just f) (Just "utf8" :: Maybe Text)) ef)
  e :: R.Event t (Maybe Text) <- R.wrapDomEvent fr (`EventM.on` FileReader.load) . JS.liftJSM $ do
    v <- FileReader.getResult fr
    (JS.fromJSVal <=< JS.toJSVal) v
  return $ R.fmapMaybe id e
