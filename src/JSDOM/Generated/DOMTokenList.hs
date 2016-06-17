{-# LANGUAGE PatternSynonyms #-}
module JSDOM.Generated.DOMTokenList
       (item, contains, add, remove, toggle, toString, getLength,
        DOMTokenList, castToDOMTokenList, gTypeDOMTokenList,
        IsDOMTokenList, toDOMTokenList)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.item Mozilla DOMTokenList.item documentation> 
item ::
     (MonadDOM m, IsDOMTokenList self, FromJSString result) =>
       self -> Word -> m (Maybe result)
item self index
  = liftDOM
      (((toDOMTokenList self) ^. jsf "item" [toJSVal index]) >>=
         fromMaybeJSString)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.contains Mozilla DOMTokenList.contains documentation> 
contains ::
         (MonadDOM m, IsDOMTokenList self, ToJSString token) =>
           self -> token -> m Bool
contains self token
  = liftDOM
      (((toDOMTokenList self) ^. jsf "contains" [toJSVal token]) >>=
         valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.add Mozilla DOMTokenList.add documentation> 
add ::
    (MonadDOM m, IsDOMTokenList self, ToJSString tokens,
     ToJSVal tokens) =>
      self -> [tokens] -> m ()
add self tokens
  = liftDOM
      (void
         ((toDOMTokenList self) ^. jsf "add" [toJSVal (array tokens)]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.remove Mozilla DOMTokenList.remove documentation> 
remove ::
       (MonadDOM m, IsDOMTokenList self, ToJSString tokens,
        ToJSVal tokens) =>
         self -> [tokens] -> m ()
remove self tokens
  = liftDOM
      (void
         ((toDOMTokenList self) ^. jsf "remove" [toJSVal (array tokens)]))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.toggle Mozilla DOMTokenList.toggle documentation> 
toggle ::
       (MonadDOM m, IsDOMTokenList self, ToJSString token) =>
         self -> token -> Bool -> m Bool
toggle self token force
  = liftDOM
      (((toDOMTokenList self) ^. jsf "toggle"
          [toJSVal token, toJSVal force])
         >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.toString Mozilla DOMTokenList.toString documentation> 
toString ::
         (MonadDOM m, IsDOMTokenList self, FromJSString result) =>
           self -> m result
toString self
  = liftDOM
      (((toDOMTokenList self) ^. js "toString") >>= fromJSValUnchecked)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList.length Mozilla DOMTokenList.length documentation> 
getLength :: (MonadDOM m, IsDOMTokenList self) => self -> m Word
getLength self
  = liftDOM
      (round <$>
         (((toDOMTokenList self) ^. js "length") >>= valToNumber))
