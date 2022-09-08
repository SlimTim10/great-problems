{-# LANGUAGE QuasiQuotes #-}
module Problem.PdfViewer
  ( widget
  ) where

import qualified Control.Monad.Loops as Loops
import qualified Text.RawString.QQ as QQ
import qualified Control.Monad.IO.Class as IO
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Control
import qualified Language.Javascript.JSaddle as JS
import qualified JSDOM.Generated.WindowOrWorkerGlobalScope as JS
import qualified JSDOM
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Api.Compile as Compile
import qualified Common.Api.Error as Error

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => R.Dynamic t (Maybe (Either Error.Error Text)) -- ^ Compile response
  -> R.Dynamic t Bool -- ^ Is loading
  -> R.Dynamic t Bool -- ^ Show errors
  -> m ()
widget compileResponse loading errorsToggle = do
  R.dyn_ $ switchView <$> compileResponse <*> loading <*> errorsToggle

switchView
  :: ( R.DomBuilder t m
     , JS.MonadJSM m
     , JS.ToJSVal (R.RawElement (R.DomBuilderSpace m))
     )
  => Maybe (Either Error.Error Text) -- ^ Compile response
  -> Bool -- ^ Is loading
  -> Bool -- ^ Show errors
  -> m ()
switchView _ True _ = loadingWidget
switchView compileResponse False True = errorsWidget compileResponse
switchView Nothing _ _ = R.text "Press compile to view"
switchView compileResponse@(Just (Left _)) _ _ = errorsWidget compileResponse
switchView (Just (Right html)) _ _ = do
  el <- Util.placeRawHTML viewerId html
  -- Need to clear MathJax so it doesn't use the previous route
  clearMathJax
  configureMathJax el
  includeMathJax el
  runMathJax
  fixMathJaxSVG el
  -- fixMathJaxSVG
  where
    includeMathJax el = Util.appendScriptURL el "text/javascript" "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_SVG"
    fixMathJaxSVG el = Util.appendScriptURL el "text/javascript" (Ob.static @"fixMathJaxSVG.js")
    configureMathJax el = Util.appendScript el "text/x-mathjax-config"
      [QQ.r|
      MathJax.Hub.Config({
        displayAlign: "center",
        displayIndent: "0em",

        "HTML-CSS": { scale: 100,
                        linebreaks: { automatic: "false" },
                        webFont: "TeX"
                       },
        SVG: {scale: 100,
              linebreaks: { automatic: "false" },
              font: "TeX"},
        NativeMML: {scale: 100},
        TeX: { equationNumbers: {autoNumber: "AMS"},
               MultLineWidth: "85%",
               TagSide: "right",
               TagIndent: ".8em"
             },
      });
      |]
    clearMathJax = JS.liftJSM $ do
      win <- JS.jsg ("window" :: Text)
      void $ win ^. JS.jss ("MathJax" :: Text) JS.jsUndefined
    runMathJax = whenMathJaxReady $ do
      mj <- JS.jsg ("MathJax" :: Text)
      hub <- mj ^. JS.js ("Hub" :: Text)
      q <- JS.toJSVal ("Typeset" :: Text, hub, viewerId)
      void $ hub ^. JS.js1 ("Queue" :: Text) q
    whenMathJaxReady f = JS.liftJSM $ do
      ctx <- JS.askJSM
      void $ IO.liftIO $ Concurrent.forkIO $ do
        Loops.untilM_ (return ()) $ do
          mjReady <- JS.runJSaddle ctx $ do
            mj <- JS.jsg ("MathJax" :: Text)
            return $ JS.isTruthy mj
          let millis = 100
          Concurrent.threadDelay (millis * 1000)
          mjReady' :: Bool <- JS.runJSaddle ctx (JS.ghcjsPure mjReady) >>= return
          return mjReady'
        void $ JS.runJSaddle ctx f
    viewerId = "problem-viewer"

errorsWidget
  :: R.DomBuilder t m
  => Maybe (Either Error.Error Text)
  -> m ()
errorsWidget Nothing = R.text ""
errorsWidget (Just (Right _)) = R.text ""
errorsWidget (Just (Left e)) = do
  R.elClass "div" "flex flex-col w-full h-full" $ do
    R.text (Error.message e)

loadingWidget
  :: R.DomBuilder t m
  => m ()
loadingWidget = R.elClass "div" "flex w-full h-full items-center justify-center" $ do
  R.elAttr "img" ("src" =: Ob.static @"pdf_spinner.svg" <> "alt" =: "loading") $ R.blank
