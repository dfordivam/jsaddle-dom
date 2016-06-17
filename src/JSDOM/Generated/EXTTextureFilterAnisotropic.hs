{-# LANGUAGE PatternSynonyms #-}
module JSDOM.Generated.EXTTextureFilterAnisotropic
       (pattern TEXTURE_MAX_ANISOTROPY_EXT,
        pattern MAX_TEXTURE_MAX_ANISOTROPY_EXT,
        EXTTextureFilterAnisotropic, castToEXTTextureFilterAnisotropic,
        gTypeEXTTextureFilterAnisotropic)
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
pattern TEXTURE_MAX_ANISOTROPY_EXT = 34046
pattern MAX_TEXTURE_MAX_ANISOTROPY_EXT = 34047
