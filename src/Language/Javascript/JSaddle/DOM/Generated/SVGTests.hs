{-# LANGUAGE PatternSynonyms #-}
module Language.Javascript.JSaddle.DOM.Generated.SVGTests
       (hasExtension, getRequiredFeatures, getRequiredExtensions,
        getSystemLanguage, SVGTests, castToSVGTests, gTypeSVGTests)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.hasExtension Mozilla SVGTests.hasExtension documentation> 
hasExtension ::
             (MonadDOM m, ToJSString extension) =>
               SVGTests -> extension -> m Bool
hasExtension self extension
  = liftDOM
      ((self ^. jsf "hasExtension" [toJSVal extension]) >>= valToBool)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredFeatures Mozilla SVGTests.requiredFeatures documentation> 
getRequiredFeatures ::
                    (MonadDOM m) => SVGTests -> m (Maybe SVGStringList)
getRequiredFeatures self
  = liftDOM ((self ^. js "requiredFeatures") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.requiredExtensions Mozilla SVGTests.requiredExtensions documentation> 
getRequiredExtensions ::
                      (MonadDOM m) => SVGTests -> m (Maybe SVGStringList)
getRequiredExtensions self
  = liftDOM ((self ^. js "requiredExtensions") >>= fromJSVal)

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests.systemLanguage Mozilla SVGTests.systemLanguage documentation> 
getSystemLanguage ::
                  (MonadDOM m) => SVGTests -> m (Maybe SVGStringList)
getSystemLanguage self
  = liftDOM ((self ^. js "systemLanguage") >>= fromJSVal)