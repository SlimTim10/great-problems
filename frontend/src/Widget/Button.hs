module Widget.Button
  ( primary
  , secondary
  , primary'
  , secondary'
  , primaryClass'
  , secondaryClass'
  , primarySmall
  , secondarySmall
  , primarySmall'
  , primarySmallClass
  , primarySmallClass'
  ) where

import Common.Lib.Prelude

import qualified Reflex.Dom.Core as R
import qualified MyReflex.Dom.Widget.Basic as R'

primary
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> m ()
primary t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "bg-brand-primary rounded text-white font-normal px-3 py-2"
  $ R.text t

secondary
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
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
  => Text -- ^ Button text
  -> m (R.Event t ())
primary' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "bg-brand-primary rounded text-white font-normal px-3 py-2"
    $ R.text t
  return $ R.domEvent R.Click e

secondary'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> m (R.Event t ())
secondary' t = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-3 py-2"
    $ R.text t
  return $ R.domEvent R.Click e

primaryClass'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> Text -- ^ Class
  -> m (R.Event t ())
primaryClass' t c = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    (cs $ "bg-brand-primary rounded text-white font-normal px-3 py-2 " ++ cs c)
    $ R.text t
  return $ R.domEvent R.Click e

secondaryClass'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> Text -- ^ Class
  -> m (R.Event t ())
secondaryClass' t c = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    (cs $ "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-3 py-2 " ++ cs c)
    $ R.text t
  return $ R.domEvent R.Click e

primarySmall
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> m ()
primarySmall t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm"
  $ R.text t

secondarySmall
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> m ()
secondarySmall t = R'.elAttrClass
  "button"
  ("type" =: "button")
  "border border-brand-primary bg-transparent rounded text-blue-700 font-medium px-2 py-1 text-brand-sm"
  $ R.text t

primarySmall'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
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
  => Text -- ^ Button text
  -> Text -- ^ Class
  -> m ()
primarySmallClass t c = R'.elAttrClass
  "button"
  ("type" =: "button")
  (cs $ "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm " ++ cs c)
  $ R.text t

primarySmallClass'
  :: forall t m.
     ( R.DomBuilder t m
     )
  => Text -- ^ Button text
  -> Text -- ^ Class
  -> m (R.Event t ())
primarySmallClass' t c = do
  (e, _) <- R'.elAttrClass'
    "button"
    ("type" =: "button")
    (cs $ "bg-brand-primary rounded text-white font-medium px-2 py-1 text-brand-sm " ++ cs c)
    $ R.text t
  return $ R.domEvent R.Click e
