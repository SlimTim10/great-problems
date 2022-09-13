{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Problem.Viewer
  ( widget
  , problemAnswerId
  , problemSolutionId
  ) where

import Prelude hiding ((!!))

import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Loops as Loops
import qualified Text.RawString.QQ as QQ
import qualified Control.Monad.IO.Class as IO
import qualified Control.Concurrent as Concurrent
import qualified Language.Javascript.JSaddle as JS
import Language.Javascript.JSaddle ((!), (!!))
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.ParentNode as DOM
import qualified GHCJS.DOM.NonElementParentNode as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM as DOM
import qualified Obelisk.Generated.Static as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util
import qualified Common.Api.Error as Error

default (Text)

viewerId :: Text
viewerId = "problem-viewer"

problemAnswerId :: Text
problemAnswerId = "outline-container-problem-answer"

problemSolutionId :: Text
problemSolutionId = "outline-container-problem-solution"

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
  
  -- This function works, but it's much slower (~1300 ms) than its vanilla JavaScript counterpart (~2 ms).
  -- So it is disabled until there is a way to make it faster or a reason to use it.
  when False fixMathJaxSVG'

  -- Hide problem answer and solution by default.
  -- In theory, this should be possible immediately after the HTML has been updated (innerHTML).
  -- However, due to reflex's inner workings, the elements cannot be found so soon.
  -- Triggering after MathJax is ready is our compromise.
  whenMathJaxReady $ do
    Util.hideElement problemAnswerId True
    Util.hideElement problemSolutionId True
  
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
      win <- JS.jsg "window"
      win ^. JS.jss "MathJax" JS.jsUndefined

    runMathJax = whenMathJaxReady $ do
      mj <- JS.jsg "MathJax"
      hub <- mj ! "Hub"
      q <- JS.toJSVal ("Typeset", hub, viewerId)
      void $ hub ^. JS.js1 "Queue" q

    whenMathJaxReady :: JS.MonadJSM m => JS.JSM () -> m ()
    whenMathJaxReady f = JS.liftJSM $ do
      ctx <- JS.askJSM
      void $ IO.liftIO $ Concurrent.forkIO $ do
        Loops.untilM_ (return ()) $ do
          mjReady <- JS.runJSaddle ctx $ do
            mj <- JS.jsg "MathJax"
            return $ JS.isTruthy mj
          let millis = 10
          Concurrent.threadDelay (millis * 1000)
          mjReady' :: Bool <- JS.runJSaddle ctx (JS.ghcjsPure mjReady) >>= return
          return mjReady'
        void $ JS.runJSaddle ctx f

    afterMathJax :: JS.MonadJSM m => JS.JSM JS.Function -> m ()
    afterMathJax jsFunc = whenMathJaxReady $ do
      mj <- JS.jsg "MathJax"
      hub <- mj ! "Hub"
      void $ hub ^. JS.js1 "Queue" jsFunc

    fixMathJaxSVG' :: JS.MonadJSM m => m ()
    fixMathJaxSVG' = afterMathJax $ JS.function $ \_ _ _ -> do
      doc <- DOM.currentDocumentUnchecked
      viewerElem <- DOM.getElementByIdUnsafe doc viewerId
      elemsToFix :: DOM.NodeList <- DOM.querySelectorAll viewerElem "svg .MathJax_SVG"
      elemsToFix' :: [DOM.Node] <- Util.nodeListNodes elemsToFix
      elemsToFix'' :: [DOM.Element] <- do
        xs <- traverse (DOM.castTo DOM.Element) elemsToFix'
        pure $ catMaybes xs
      for_ elemsToFix'' fixElem

    fixElem :: DOM.Element -> JS.JSM ()
    fixElem el = void $ MaybeT.runMaybeT $ do
      textElem :: DOM.Element <- MaybeT.MaybeT $ DOM.closest el "text"
      x <- JS.liftJSM $ textElem ! "x" ! "baseVal" !! 0 ! "value"
      y <- JS.liftJSM $ textElem ! "y" ! "baseVal" !! 0 ! "value"
      fontSize <- JS.liftJSM $ textElem ! "style" ! "font-size"
      anchor :: Text <- MaybeT.MaybeT $ JS.fromJSVal =<<
        textElem ! "style" ! "text-anchor"
      svgElem <- MaybeT.MaybeT $ DOM.querySelector el "svg"
      JS.liftJSM $ svgElem ! "style" ^. JS.jss "font-size" fontSize
      void $ JS.liftJSM $ svgElem ^. JS.js2 "setAttribute" "x" x
      void $ JS.liftJSM $ svgElem ^. JS.js2 "setAttribute" "y" y
      -- Adjust position based on anchor
      widthPx :: Double <- MaybeT.MaybeT $ JS.fromJSVal =<<
        svgElem ! "width" ! "baseVal" ! "value"
      heightPx :: Double <- MaybeT.MaybeT $ JS.fromJSVal =<<
        svgElem ! "height" ! "baseVal" ! "value"
      let translateY = "translateY(-" <> (cs . show $ heightPx / 2) <> "px)"
      let translateX = case anchor of
            "middle" -> "translateX(-" <> (cs . show $ widthPx / 2) <> "px)"
            "end" -> "translateX(-" <> (cs . show $ widthPx) <> "px)"
            _ -> ""
      JS.liftJSM $ svgElem ! "style" ^. JS.jss "transform" (translateX <> " " <> translateY)
      JS.liftJSM $ replaceElem textElem svgElem

    replaceElem :: DOM.Element -> DOM.Element -> JS.JSM ()
    replaceElem old new = void $ MaybeT.runMaybeT $ do
      parentNode :: DOM.Node <- MaybeT.MaybeT $ DOM.getParentNode old
      DOM.replaceChild_ parentNode new old

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
