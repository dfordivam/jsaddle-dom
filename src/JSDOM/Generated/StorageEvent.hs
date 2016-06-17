{-# LANGUAGE PatternSynonyms #-}
module JSDOM.Generated.StorageEvent
       (initStorageEvent, getKey, getOldValue, getNewValue, getUrl,
        getStorageArea, StorageEvent, castToStorageEvent,
        gTypeStorageEvent)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent.initStorageEvent Mozilla StorageEvent.initStorageEvent documentation> 
initStorageEvent ::
                 (MonadDOM m, ToJSString typeArg, ToJSString keyArg,
                  ToJSString oldValueArg, ToJSString newValueArg,
                  ToJSString urlArg) =>
                   StorageEvent ->
                     typeArg ->
                       Bool ->
                         Bool ->
                           keyArg ->
                             Maybe oldValueArg ->
                               Maybe newValueArg -> urlArg -> Maybe Storage -> m ()
initStorageEvent self typeArg canBubbleArg cancelableArg keyArg
  oldValueArg newValueArg urlArg storageAreaArg
  = liftDOM
      (void
         (self ^. jsf "initStorageEvent"
            [toJSVal typeArg, toJSVal canBubbleArg, toJSVal cancelableArg,
             toJSVal keyArg, toJSVal oldValueArg, toJSVal newValueArg,
             toJSVal urlArg, toJSVal storageAreaArg]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent.key Mozilla StorageEvent.key documentation> 
getKey ::
       (MonadDOM m, FromJSString result) => StorageEvent -> m result
getKey self = liftDOM ((self ^. js "key") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent.oldValue Mozilla StorageEvent.oldValue documentation> 
getOldValue ::
            (MonadDOM m, FromJSString result) =>
              StorageEvent -> m (Maybe result)
getOldValue self
  = liftDOM ((self ^. js "oldValue") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent.newValue Mozilla StorageEvent.newValue documentation> 
getNewValue ::
            (MonadDOM m, FromJSString result) =>
              StorageEvent -> m (Maybe result)
getNewValue self
  = liftDOM ((self ^. js "newValue") >>= fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent.url Mozilla StorageEvent.url documentation> 
getUrl ::
       (MonadDOM m, FromJSString result) => StorageEvent -> m result
getUrl self = liftDOM ((self ^. js "url") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent.storageArea Mozilla StorageEvent.storageArea documentation> 
getStorageArea :: (MonadDOM m) => StorageEvent -> m (Maybe Storage)
getStorageArea self
  = liftDOM ((self ^. js "storageArea") >>= fromJSVal)
