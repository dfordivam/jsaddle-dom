{-# LANGUAGE PatternSynonyms #-}
module Language.Javascript.JSaddle.DOM.Generated.Notification
       (show, showEvent, error, close, click, Notification,
        castToNotification, gTypeNotification)
       where
import Prelude ((.), (==), (>>=), return, IO, Int, Float, Double, Bool(..), Maybe, maybe, fromIntegral, round, realToFrac, fmap, Show, Read, Eq, Ord, Maybe(..))
import Data.Typeable (Typeable)
import Language.Javascript.JSaddle (JSM(..), JSVal(..), JSString, strictEqual, toJSVal, valToStr, valToNumber, valToBool, js, jss, jsf, jsg, function, new, array)
import Data.Int (Int64)
import Data.Word (Word, Word64)
import Language.Javascript.JSaddle.DOM.Types
import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Lens.Operators ((^.))
import Language.Javascript.JSaddle.DOM.EventTargetClosures (EventName, unsafeEventName)
import Language.Javascript.JSaddle.DOM.Enums

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Notification.show Mozilla Notification.show documentation> 
show :: (MonadDOM m) => Notification -> m ()
show self = liftDOM (void (self ^. js "show"))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Notification.onshow Mozilla Notification.onshow documentation> 
showEvent :: EventName Notification MouseEvent
showEvent = unsafeEventName (toJSString "show")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Notification.onerror Mozilla Notification.onerror documentation> 
error :: EventName Notification UIEvent
error = unsafeEventName (toJSString "error")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Notification.onclose Mozilla Notification.onclose documentation> 
close :: EventName Notification Event
close = unsafeEventName (toJSString "close")

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Notification.onclick Mozilla Notification.onclick documentation> 
click :: EventName Notification MouseEvent
click = unsafeEventName (toJSString "click")