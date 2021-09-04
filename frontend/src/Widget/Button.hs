module Widget.Button
  ( primary
  , secondary
  , primary'
  , primaryClass'
  , primarySmall
  , primarySmall'
  , primarySmallClass
  , primarySmallClass'
  , secondary'
  ) where

import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

import Global

primary
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
primary t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "bg-brand-primary rounded text-white font-medium px-3 py-2"
  $ R.text t

secondary
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
secondary t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-3 py-2"
  $ R.text t

primary'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m (R.Event t ())
primary' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "bg-brand-primary rounded text-white font-medium px-3 py-2"
    $ R.text t
  return $ R.domEvent R.Click e

primaryClass'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> Text
  -> m (R.Event t ())
primaryClass' c t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    (cs $ "bg-brand-primary rounded text-white font-medium px-3 py-2 " ++ cs c)
    $ R.text t
  return $ R.domEvent R.Click e

primarySmall
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m ()
primarySmall t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm"
  $ R.text t

primarySmall'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m (R.Event t ())
primarySmall' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm"
    $ R.text t
  return $ R.domEvent R.Click e

primarySmallClass
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> Text
  -> m ()
primarySmallClass c t = R'.elAttrClass
  "button"
  ("type" =: "button")
  (cs $ "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm " ++ cs c)
  $ R.text t

primarySmallClass'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Style
  -> Text
  -> m (R.Event t ())
primarySmallClass' c t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    (cs $ "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm " ++ cs c)
    $ R.text t
  return $ R.domEvent R.Click e

secondary'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text
  -> m (R.Event t ())
secondary' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-3 py-2"
    $ R.text t
  return $ R.domEvent R.Click e
