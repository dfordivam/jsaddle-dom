module Language.Javascript.JSaddle.DOM.EventTargetClosures
       (EventName(..), SaferEventListener(..), unsafeEventName, eventListenerNew, eventListenerNewSync, eventListenerNewAsync, eventListenerRelease) where

import Control.Applicative ((<$>))
import Language.Javascript.JSaddle.DOM.Types
import Language.Javascript.JSaddle (JSUndefined, function, JSM, Function(..), freeFunction)
import Control.Monad.IO.Class (MonadIO(..))

newtype EventName t e = EventName DOMString
newtype SaferEventListener t e = SaferEventListener Function

instance ToJSVal (SaferEventListener t e) where
    toJSVal (SaferEventListener l) = toJSVal l
    {-# INLINE toJSVal #-}

--instance FromJSVal (SaferEventListener t e) where
--    fromJSVal l = fmap SaferEventListener <$> fromJSVal l
--    {-# INLINE fromJSVal #-}

unsafeEventName :: DOMString -> EventName t e
unsafeEventName = EventName

eventListenerNew :: (IsEvent e) => (e -> JSM ()) -> JSM (SaferEventListener t e)
eventListenerNew callback = SaferEventListener <$> function "listener" (\_ _ [e] -> fromJSValUnchecked e >>= callback)

eventListenerNewSync :: (IsEvent e) => (e -> JSM ()) -> JSM (SaferEventListener t e)
eventListenerNewSync callback = SaferEventListener <$> function "listener" (\_ _ [e] -> fromJSValUnchecked e >>= callback)

eventListenerNewAsync :: (IsEvent e) => (e -> JSM ()) -> JSM (SaferEventListener t e)
eventListenerNewAsync callback = SaferEventListener <$> function "listener" (\_ _ [e] -> fromJSValUnchecked e >>= callback)

eventListenerRelease :: SaferEventListener t e -> JSM ()
eventListenerRelease (SaferEventListener f) = freeFunction f
