{-# LANGUAGE PatternSynonyms #-}
module JSDOM.Generated.HTMLFormControlsCollection
       (_get, namedItem, HTMLFormControlsCollection,
        castToHTMLFormControlsCollection, gTypeHTMLFormControlsCollection)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormControlsCollection._get Mozilla HTMLFormControlsCollection._get documentation> 
_get ::
     (MonadDOM m) =>
       HTMLFormControlsCollection -> Word -> m (Maybe Node)
_get self index
  = liftDOM ((self ^. jsf "_get" [toJSVal index]) >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormControlsCollection.namedItem Mozilla HTMLFormControlsCollection.namedItem documentation> 
namedItem ::
          (MonadDOM m, ToJSString name) =>
            HTMLFormControlsCollection -> name -> m (Maybe Node)
namedItem self name
  = liftDOM ((self ^. jsf "namedItem" [toJSVal name]) >>= fromJSVal)
