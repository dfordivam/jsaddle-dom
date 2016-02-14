{-# LANGUAGE PatternSynonyms #-}
module Language.Javascript.JSaddle.DOM.Generated.SVGPathSegLinetoRel
       (setX, getX, setY, getY, SVGPathSegLinetoRel,
        castToSVGPathSegLinetoRel, gTypeSVGPathSegLinetoRel)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoRel.x Mozilla SVGPathSegLinetoRel.x documentation> 
setX :: (MonadDOM m) => SVGPathSegLinetoRel -> Float -> m ()
setX self val = liftDOM (self ^. jss "x" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoRel.x Mozilla SVGPathSegLinetoRel.x documentation> 
getX :: (MonadDOM m) => SVGPathSegLinetoRel -> m Float
getX self
  = liftDOM (realToFrac <$> ((self ^. js "x") >>= valToNumber))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoRel.y Mozilla SVGPathSegLinetoRel.y documentation> 
setY :: (MonadDOM m) => SVGPathSegLinetoRel -> Float -> m ()
setY self val = liftDOM (self ^. jss "y" (toJSVal val))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoRel.y Mozilla SVGPathSegLinetoRel.y documentation> 
getY :: (MonadDOM m) => SVGPathSegLinetoRel -> m Float
getY self
  = liftDOM (realToFrac <$> ((self ^. js "y") >>= valToNumber))