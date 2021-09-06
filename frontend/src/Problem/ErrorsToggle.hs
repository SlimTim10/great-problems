module Problem.ErrorsToggle
  ( widget
  ) where

import qualified Data.Maybe as Maybe
import qualified Data.Text as T

import qualified Reflex.Dom.Core as R

import qualified Common.Compile as Compile
import qualified Util
import Global

widget
  :: forall t m a.
     ( R.DomBuilder t m
     , R.MonadHold t m
     , MonadFix m
     , R.PostBuild t m
     )
  => R.Dynamic t (Maybe Compile.CompileResponse) -- ^ Response from compile attempt, which should contain the errors to show
  -> R.Event t a -- ^ Force reset
  -> m (R.Dynamic t Bool)
widget compileResponse reset = do
  rec
    e <- Util.buttonDynClass "Error log" $ style <$> btnOrReset <*> hasErrors
    btnOrReset <- R.foldDyn ($) False $ R.leftmost [not <$ e, const False <$ reset]
  return $ showErrors <$> btnOrReset <*> compileResponse
  where
    hasErrors :: R.Dynamic t Bool = f <$> compileResponse
    f :: Maybe Compile.CompileResponse -> Bool
    f Nothing = False
    f (Just res) = not . T.null $ Compile.errorIcemaker res
    style
      :: Bool -- ^ Active toggled
      -> Bool -- ^ Errors are present
      -> Text
    style True _ = "border border-brand-primary rounded bg-brand-primary text-white font-medium px-2 py-1 text-brand-sm"
    style False False = "border border-brand-primary rounded bg-white text-blue-700 font-medium px-2 py-1 text-brand-sm"
    style False True = "border border-brand-primary rounded bg-red-200 text-blue-700 font-medium px-2 py-1 text-brand-sm"

showErrors
  :: Bool
  -> Maybe Compile.CompileResponse
  -> Bool
showErrors btn compileResponse
  | btn = True
  | Maybe.isNothing compileResponse = False
  | not . T.null $ pdfContent = False
  | T.null pdfContent = True
  | otherwise = False
  where
    pdfContent = Compile.pdfContent . Maybe.fromJust $ compileResponse
