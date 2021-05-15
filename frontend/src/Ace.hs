-- Taken from
-- https://github.com/reflex-frp/reflex-dom-ace
-- with modifications

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|

Basic support for using the Ace editor with Reflex.

IMPORTANT NOTE:

This currently does not work if your app is using reflex-dom's
mainWidgetWithHead or mainWidgetWithCss.

Example usage:

    ace <- divClass "yourAceWrapperDiv" $ -- wrapper div not required
      aceWidget def (AceDynConfig Nothing) never "initial editor contents"

    -- The rest is optional and lets you change what's in the editor on the fly
    -- fly without redrawing the widget.
    withAceInstance ace (setValueAce <$> updatesToContents)
    holdDyn iv $ leftmost
      [ updatesToContents
      , updated (aceValue ace)
      ]

-}

module Ace where

------------------------------------------------------------------------------
import           Control.Lens                       ((^.))
import           Control.Monad                      (unless, void, join)
import           Control.Monad.Trans
import           Data.Bifunctor (bimap)
import           Data.Default
import           Data.Map                           (Map)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           GHCJS.DOM.Types                    (JSVal)
import           Language.Javascript.JSaddle        (asyncFunction,
                                                     fromJSValUnchecked, js,
                                                     js0, js1, js2, jsg)
import           Language.Javascript.JSaddle.Object (MakeObject (..), create,
                                                     (<#))
import           Language.Javascript.JSaddle.Types  (MonadJSM, liftJSM)
import           Language.Javascript.JSaddle.Value  (ToJSVal (..))
import           Reflex
import           Reflex.Dom.Core                    hiding (Element,
                                                     fromJSString)
------------------------------------------------------------------------------


data AceTheme
  = AceTheme_Chrome
  | AceTheme_Clouds
  | AceTheme_CrimsonEditor
  | AceTheme_Dawn
  | AceTheme_Dreamweaver
  | AceTheme_Eclipse
  | AceTheme_Github
  | AceTheme_Iplastic
  | AceTheme_SolarizedLight
  | AceTheme_Textmate
  | AceTheme_Tomorrow
  | AceTheme_Xcode
  | AceTheme_Kuroir
  | AceTheme_Katzenmilch
  | AceTheme_Sqlserver
  | AceTheme_Ambiance
  | AceTheme_Chaos
  | AceTheme_CloudsMidnight
  | AceTheme_Cobalt
  | AceTheme_Gruvbox
  | AceTheme_IdleFingers
  | AceTheme_KrTheme
  | AceTheme_Merbivore
  | AceTheme_MerbivoreSoft
  | AceTheme_MonoIndustrial
  | AceTheme_Monokai
  | AceTheme_PastelOnDark
  | AceTheme_SolarizedDark
  | AceTheme_Terminal
  | AceTheme_TomorrowNight
  | AceTheme_TomorrowNightBlue
  | AceTheme_TomorrowNightBright
  | AceTheme_TomorrowNightEighties
  | AceTheme_Twilight
  | AceTheme_VibrantInk
  deriving (Eq,Ord,Enum,Bounded)

instance Show AceTheme where
    show AceTheme_Ambiance              = "ambiance"
    show AceTheme_Chaos                 = "chaos"
    show AceTheme_Chrome                = "chrome"
    show AceTheme_Clouds                = "clouds"
    show AceTheme_CloudsMidnight        = "clouds_midnight"
    show AceTheme_Cobalt                = "cobalt"
    show AceTheme_CrimsonEditor         = "crimson_editor"
    show AceTheme_Dawn                  = "dawn"
    show AceTheme_Dreamweaver           = "dreamweaver"
    show AceTheme_Eclipse               = "eclipse"
    show AceTheme_Github                = "github"
    show AceTheme_Gruvbox               = "gruvbox"
    show AceTheme_IdleFingers           = "idle_fingers"
    show AceTheme_Iplastic              = "iplastic"
    show AceTheme_Katzenmilch           = "katzenmilch"
    show AceTheme_KrTheme               = "kr_theme"
    show AceTheme_Kuroir                = "kuroir"
    show AceTheme_Merbivore             = "merbivore"
    show AceTheme_MerbivoreSoft         = "merbivore_soft"
    show AceTheme_MonoIndustrial        = "mono_industrial"
    show AceTheme_Monokai               = "monokai"
    show AceTheme_PastelOnDark          = "pastel_on_dark"
    show AceTheme_SolarizedDark         = "solarized_dark"
    show AceTheme_SolarizedLight        = "solarized_light"
    show AceTheme_Sqlserver             = "sqlserver"
    show AceTheme_Terminal              = "terminal"
    show AceTheme_Textmate              = "textmate"
    show AceTheme_Tomorrow              = "tomorrow"
    show AceTheme_TomorrowNight         = "tomorrow_night"
    show AceTheme_TomorrowNightBlue     = "tomorrow_night_blue"
    show AceTheme_TomorrowNightBright   = "tomorrow_night_bright"
    show AceTheme_TomorrowNightEighties = "tomorrow_night_eighties"
    show AceTheme_Twilight              = "twilight"
    show AceTheme_VibrantInk            = "vibrant_ink"
    show AceTheme_Xcode                 = "xcode"


data AceConfig = AceConfig
    { _aceConfigElemAttrs       :: Map Text Text
    , _aceConfigBasePath        :: Maybe Text
    , _aceConfigMode            :: Maybe Text
    , _aceConfigWordWrap        :: Bool
    , _aceConfigShowPrintMargin :: Bool
    }


data AceDynConfig = AceDynConfig
    { _aceDynConfigTheme :: Maybe AceTheme
    }


instance Default AceConfig where
    def = AceConfig def def def False False


newtype AceInstance = AceInstance { unAceInstance :: JSVal }


data Ace t = Ace
    { aceRef   :: Dynamic t (Maybe AceInstance)
    , aceValue :: Dynamic t Text
    }


------------------------------------------------------------------------------
-- The type of editor session line annotation.
data AnnotationType = AnnotationError
                    | AnnotationWarning
                    deriving (Show, Read)

------------------------------------------------------------------------------
instance ToJSVal AnnotationType where
  toJSVal AnnotationError   = toJSVal ("error":: Text)
  toJSVal AnnotationWarning = toJSVal ("warning" :: Text)


------------------------------------------------------------------------------
-- A line annotation for marking a specific line within the editor session as
-- an error or a warning.
data Annotation = Annotation { annotationRow    :: Int
                             , annotationColumn :: Int
                             , annotationText   :: Text
                             , annotationType   :: AnnotationType
                             } deriving (Show, Read)


------------------------------------------------------------------------------
instance MakeObject Annotation where
  makeObject (Annotation row col txt typ) = do
    o <- create
    (o <# ("row" :: Text)   ) row
    (o <# ("column" :: Text)) col
    (o <# ("text" :: Text)  ) txt
    (o <# ("type" :: Text)  ) typ
    return o


instance ToJSVal Annotation where
  toJSVal = (toJSVal =<<) . makeObject


------------------------------------------------------------------------------
startAce :: MonadJSM m => Text -> AceConfig -> m AceInstance
startAce containerId ac = liftJSM $ do
  aceVal <- jsg ("ace" :: Text)
  let
    (basePath, mode) = join bimap (fromMaybe (T.pack "") . ($ ac))
      (_aceConfigBasePath, _aceConfigMode)
  -- Set the base path if given
  unless (T.null basePath) $ do
    config <- aceVal ^. js ("config" :: Text)
    void $ config ^. js2 ("set" :: Text) ("basePath" :: Text) basePath
  -- Start and return an editing session
  editor <- aceVal ^. js1 ("edit" :: Text) containerId
  let aceInst = AceInstance editor
  -- Set the mode if given
  unless (T.null mode) $ do
    setModeAce mode aceInst
  setUseWrapMode (_aceConfigWordWrap ac) aceInst
  setShowPrintMargin (_aceConfigShowPrintMargin ac) aceInst
  return aceInst


------------------------------------------------------------------------------
moveCursorToPosition :: MonadJSM m => (Int, Int) -> AceInstance -> m ()
moveCursorToPosition (r, c) (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 ("gotoLine" :: Text) r c


------------------------------------------------------------------------------
setThemeAce :: MonadJSM m => Maybe AceTheme -> AceInstance -> m ()
setThemeAce Nothing      _                 = return ()
setThemeAce (Just theme) (AceInstance ace) =
  liftJSM $ void $ ace ^. js1 ("setTheme" :: Text) themeStr
  where themeStr = "ace/theme/" <> show theme


------------------------------------------------------------------------------
setModeAce :: MonadJSM m => Text -> AceInstance -> m ()
setModeAce mode (AceInstance ace) = liftJSM $ do
  session <- ace ^. js ("session" :: Text)
  void $ session ^. js1 ("setMode" :: Text) modeStr
  where modeStr = "ace/mode/" <> mode


------------------------------------------------------------------------------
setUseWrapMode :: MonadJSM m => Bool -> AceInstance -> m ()
setUseWrapMode shouldWrap (AceInstance ace) = liftJSM $ do
  session <- ace ^. js0 ("getSession" :: Text)
  void $ session ^. js1 ("setUseWrapMode" :: Text) shouldWrap


------------------------------------------------------------------------------
setShowPrintMargin :: MonadJSM m => Bool -> AceInstance -> m ()
setShowPrintMargin shouldShow (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 ("setOption" :: Text) ("showPrintMargin" :: Text) shouldShow


------------------------------------------------------------------------------
setUseWorker :: MonadJSM m => Bool -> AceInstance -> m ()
setUseWorker shouldUse (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 ("setOption" :: Text) ("useWorker" :: Text) shouldUse


------------------------------------------------------------------------------
setAnnotations :: MonadJSM m => [Annotation] -> AceInstance -> m ()
setAnnotations as (AceInstance ace) = liftJSM $ do
  session <- ace ^. js0 ("getSession" :: Text)
  annotations <- toJSValListOf as
  void $ session ^. js1 ("setAnnotations" :: Text) annotations


------------------------------------------------------------------------------
setConfigAce :: MonadJSM m => Text -> Text -> AceInstance -> m ()
setConfigAce t1 t2 (AceInstance ace) = liftJSM $ do
  cfg <- ace ^. js ("config" :: Text)
  void $ cfg ^. js2 ("set" :: Text) t1 t2


------------------------------------------------------------------------------
getValueAce :: MonadJSM m => AceInstance -> m Text
getValueAce (AceInstance ace) =
  liftJSM $ ace ^. js0 ("getValue" :: Text) >>= fromJSValUnchecked


------------------------------------------------------------------------------
setValueAce :: MonadJSM m => Text -> AceInstance -> m ()
setValueAce t (AceInstance ace) =
  liftJSM $ void $ ace ^. js2 ("setValue" :: Text) t (-1 :: Int)


------------------------------------------------------------------------------
setupValueListener
  :: ( MonadJSM (Performable m)
     , DomBuilder t m
     , PostBuild t m
     , TriggerEvent t m
     , PerformEvent t m
     )
  => AceInstance
  -> m (Event t Text)
setupValueListener (AceInstance ace) = do
  pb  <- getPostBuild
  let act cb = liftJSM $ do
        jscb <- asyncFunction $ \_ _ _ ->
          getValueAce (AceInstance ace) >>= liftIO . cb
        void $ ace ^. js2 ("on" :: Text) ("change" :: Text) jscb
  performEventAsync (act <$ pb)


------------------------------------------------------------------------------
-- | Main entry point
--
-- IMPORTANT NOTE:
--
-- This currently does not work if your app is using reflex-dom's
-- mainWidgetWithHead or mainWidgetWithCss.
aceWidget
    :: ( DomBuilder t m
       , PostBuild t m
       , MonadHold t m
       , MonadJSM m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadJSM (Performable m)
       )
    => AceConfig
    -> AceDynConfig
    -> Event t AceDynConfig
    -> Text
    -> Text
    -> Event t Text
    -> m (Ace t)
aceWidget ac adc adcUps containerId initContents contentsUps = do
    aceInstance <- startAce containerId ac
    onChange <- setupValueListener aceInstance
    updatesDyn <- holdDyn initContents onChange

    let ace = Ace (constDyn $ pure aceInstance) updatesDyn
    setThemeAce (_aceDynConfigTheme adc) aceInstance
    void $ withAceInstance ace (setThemeAce . _aceDynConfigTheme <$> adcUps)
    performEvent_ $ ffor contentsUps $ \c -> setValueAce c aceInstance
    return ace


------------------------------------------------------------------------------
-- | Convenient helper function for running functions that need an AceInstance.
withAceInstance
    :: PerformEvent t m
    => Ace t
    -> Event t (AceInstance -> Performable m ())
    -> m (Event t ())
withAceInstance ace evt = withAceInstance' ace (f <$> evt)
  where
    f _ Nothing  = return ()
    f g (Just a) = g a


------------------------------------------------------------------------------
-- | More powerful function for running functions that need an AceInstance.
withAceInstance'
    :: PerformEvent t m
    => Ace t
    -> Event t (Maybe AceInstance -> Performable m a)
    -> m (Event t a)
withAceInstance' ace =
  performEvent . attachPromptlyDynWith (flip ($)) (aceRef ace)
