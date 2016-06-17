{-# LANGUAGE PatternSynonyms #-}
module JSDOM.Generated.MessageChannel
       (newMessageChannel, getPort1, getPort2, MessageChannel,
        castToMessageChannel, gTypeMessageChannel)
       where
import Prelude ((.), (==), (>>=), return, IO, Int, Float, Double, Bool(..), Maybe, maybe, fromIntegral, round, realToFrac, fmap, Show, Read, Eq, Ord, Maybe(..))
import Data.Typeable (Typeable)
import Language.Javascript.JSaddle (JSM(..), JSVal(..), JSString, strictEqual, toJSVal, valToStr, valToNumber, valToBool, js, jss, jsf, jsg, function, new, array)
import Data.Int (Int64)
import Data.Word (Word, Word64)
import JSDOM.Types
import Control.Applicative ((<$>))
import Control.Monad (void)
import Control.Lens.Operators ((^.))
import JSDOM.EventTargetClosures (EventName, unsafeEventName)
import JSDOM.Enums

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel Mozilla MessageChannel documentation> 
newMessageChannel :: (MonadDOM m) => m MessageChannel
newMessageChannel
  = liftDOM (MessageChannel <$> new (jsg "MessageChannel") ())

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel.port1 Mozilla MessageChannel.port1 documentation> 
getPort1 :: (MonadDOM m) => MessageChannel -> m (Maybe MessagePort)
getPort1 self = liftDOM ((self ^. js "port1") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel.port2 Mozilla MessageChannel.port2 documentation> 
getPort2 :: (MonadDOM m) => MessageChannel -> m (Maybe MessagePort)
getPort2 self = liftDOM ((self ^. js "port2") >>= fromJSVal)
