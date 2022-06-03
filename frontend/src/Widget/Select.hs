module Widget.Select
  -- ( widget
  ( widgetWithAny
  , DropdownItem
  , DropdownKey(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Aeson as JSON
import qualified Language.Javascript.JSaddle as JS
import qualified Obelisk.Route.Frontend as Ob
import qualified Reflex.Dom.Core as R

import Common.Lib.Prelude
import qualified Common.Route as Route
import qualified Frontend.Lib.Util as Util
import qualified Widget.Input as Input

type DropdownItem a = Map (DropdownKey a) Text

data DropdownKey a = DropdownKey
  { dkIdx :: Integer
  , dkValue :: a
  } deriving (Eq)

instance Eq a => Ord (DropdownKey a) where
  a <= b = dkIdx a <= dkIdx b

widgetWithAny
  :: forall t m a b c.
     ( R.DomBuilder t m
     , R.HasJSContext (R.Performable m)
     , JS.MonadJSM (R.Performable m)
     , R.PostBuild t m
     , JS.MonadJSM m
     , R.PerformEvent t m
     , R.TriggerEvent t m
     , R.MonadHold t m
     , MonadFix m
     , Eq a
     , JSON.FromJSON b
     )
  => R.Event t (Maybe a) -- ^ Set selected item by value
  -> Text -- ^ Dropdown title
  -> Ob.R Route.Api -- ^ Route to fetch values to populate
  -> ([b] -> [c]) -- ^ Transform the fetched values before adding to dropdown (use id if not needed)
  -> ((Integer, c) -> DropdownItem a -> DropdownItem a) -- ^ How to add an item to the dropdown
  -> a -- ^ Value for "Any" item
  -> m (R.Dynamic t (Maybe a)) -- ^ Selected dropdown value
widgetWithAny setValue title route transformValues addItem anyValue = R.elClass "div" "" $ do
  R.elClass "p" "font-medium mb-2" $ R.text title
  response :: R.Event t (Maybe [b]) <- Util.getOnload $
    Route.apiHref route
  let allValues :: R.Event t [b] = fromMaybe [] <$> response
  dropdownItems :: R.Dynamic t (DropdownItem a) <- R.holdDyn Map.empty $
    valuesToDropdownItems
    . transformValues
    <$> allValues
  -- Every dropdown item needs an index and user ID, so we use -1 for the "Any" item (default)
  let anyIdx = -1
  let anyItem = DropdownKey anyIdx anyValue
  let defaultItem = anyItem
  let setValue' :: R.Event t a = fromMaybe anyValue <$> setValue
  let dropdownItems' = Map.insert anyItem "Any" <$> dropdownItems
  let dropdownKeys :: R.Dynamic t [DropdownKey a] = Map.keys <$> dropdownItems'
  holdSetValue :: R.Dynamic t a <- R.holdDyn anyValue setValue'
  let setValueKey :: R.Dynamic t (DropdownKey a) =
        (\v -> fromMaybe defaultItem . find ((== v) . dkValue))
        <$> holdSetValue <*> dropdownKeys
  x <- Input.dropdownClass' "border border-brand-light-gray w-full"
    defaultItem
    dropdownItems'
    (R.updated setValueKey)
  return $ x <&> \ddk -> do
    let v = dkValue ddk
    if v == anyValue
      then Nothing
      else Just v
      
  where
    valuesToDropdownItems :: [c] -> DropdownItem a
    valuesToDropdownItems = foldr addItem mempty . zip [1 ..]
