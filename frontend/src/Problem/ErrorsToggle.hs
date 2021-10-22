module Problem.ErrorsToggle
  ( widget
  ) where

import Common.Lib.Prelude
import qualified Frontend.Lib.Util as Util

import qualified Data.Text as T
import qualified Reflex.Dom.Core as R

import qualified Common.Api.Compile as Compile

widget
  :: forall t m a.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , MonadFix m
     , R.PostBuild t m
     )
  => R.Dynamic t (Maybe Compile.Response) -- ^ Response from compile attempt, which should contain the errors to show
  -> R.Event t a -- ^ Force reset
  -> m (R.Dynamic t Bool)
widget compileResponse reset = do
  rec
    e <- Util.buttonDynClass "Error log" $ style <$> btnOrReset <*> compileResponse <*> hasErrors
    btnOrReset <- R.foldDyn ($) False $ R.leftmost [not <$ e, const False <$ reset]
  return $ showErrors <$> btnOrReset <*> compileResponse
  where
    hasErrors :: R.Dynamic t Bool = f <$> compileResponse
    f :: Maybe Compile.Response -> Bool
    f Nothing = False
    f (Just res) = not . T.null $ Compile.resErrorProblem2tex res
    style
      :: Bool -- ^ Active toggled
      -> Maybe Compile.Response -- ^ Response from compile request
      -> Bool -- ^ Errors are present
      -> Text
    style activeToggled compileResponse' hasErrors'
      | hasErrors' == True && (activeToggled == False || T.null pdfContents) =
          "border border-brand-primary rounded bg-red-200 text-blue-700 font-medium px-2 py-1 text-brand-sm"
      | activeToggled == True =
          "border border-brand-primary rounded bg-brand-primary text-white font-medium px-2 py-1 text-brand-sm"
      | otherwise =
          "border border-brand-primary rounded bg-white text-blue-700 font-medium px-2 py-1 text-brand-sm"
      where
        pdfContents = maybe T.empty Compile.resPdfContents compileResponse'

showErrors
  :: Bool
  -> Maybe Compile.Response
  -> Bool
showErrors activeToggled compileResponse
  | activeToggled = True
  | isNothing compileResponse = False
  | not . T.null $ pdfContents = False
  | T.null pdfContents = True
  | otherwise = False
  where
    pdfContents = maybe T.empty Compile.resPdfContents compileResponse
