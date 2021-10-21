module Problem.Options
  ( widget
  , Options(..)
  ) where

import Frontend.Lib.Prelude

import qualified Control.Monad.Fix as Fix
import qualified Data.Map as Map

import qualified Reflex.Dom.Core as R

data Options = Options
  { random :: Bool
  , output :: Text
  }

widget
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     )
  => m (R.Dynamic t Options)
widget = do
  R.elClass "p" "text-xl bg-blue-200" $ R.text "Options"
  (r, o) <- R.elClass "ul" "flex flex-col divide-y" $ do
    r :: R.Dynamic t Bool <- randomOption
    o :: R.Dynamic t Text <- outputOption
    return (r, o)
  let options = Options <$> r <*> o
  R.holdDyn (Options False "") $ R.updated options

randomOption :: (R.DomBuilder t m) => m (R.Dynamic t Bool)
randomOption = R.el "li" $ do
  cb <- R.el "label" $ do
    cb1 <- R.inputElement $ R.def & R.initialAttributes .~ (
      "type" =: "checkbox"
      )
    R.text "Randomize variables"
    return cb1
  return $ R._inputElement_checked cb

outputOption
  :: ( R.DomBuilder t m
     , R.PostBuild t m
     , R.MonadHold t m
     , Fix.MonadFix m
     )
  => m (R.Dynamic t Text)
outputOption = R.el "li" $ do
  dd <- R.el "label" $ do
    R.text "Output"
    dd1 <- R.dropdown "flagSolutions" (R.constDyn items) R.def
    return dd1
  return $ R.value dd
  where
    items :: Map Text Text
    items = Map.fromList
      [ ("flagSolutions", "with solutions")
      , ("flagAnswers", "with answer")
      , ("flagSolAns", "with solution and answer")
      , ("flagQuestions", "questions only")
      ]
