{-# LANGUAGE PatternSynonyms #-}
module Language.Javascript.JSaddle.DOM.Generated.PerformanceNavigation
       (pattern TYPE_NAVIGATE, pattern TYPE_RELOAD,
        pattern TYPE_BACK_FORWARD, pattern TYPE_RESERVED, getType,
        getRedirectCount, PerformanceNavigation,
        castToPerformanceNavigation, gTypePerformanceNavigation)
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
pattern TYPE_NAVIGATE = 0
pattern TYPE_RELOAD = 1
pattern TYPE_BACK_FORWARD = 2
pattern TYPE_RESERVED = 255

-- | <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigation.type Mozilla PerformanceNavigation.type documentation> 
getType :: (MonadDOM m) => PerformanceNavigation -> m Word
getType self
  = liftDOM (round <$> ((self ^. js "type") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigation.redirectCount Mozilla PerformanceNavigation.redirectCount documentation> 
getRedirectCount :: (MonadDOM m) => PerformanceNavigation -> m Word
getRedirectCount self
  = liftDOM
      (round <$> ((self ^. js "redirectCount") >>= valToNumber))