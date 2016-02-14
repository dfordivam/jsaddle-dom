{-# LANGUAGE PatternSynonyms #-}
module Language.Javascript.JSaddle.DOM.Generated.SQLTransactionErrorCallback
       (newSQLTransactionErrorCallback,
        newSQLTransactionErrorCallbackSync,
        newSQLTransactionErrorCallbackAsync, SQLTransactionErrorCallback)
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

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SQLTransactionErrorCallback Mozilla SQLTransactionErrorCallback documentation> 
newSQLTransactionErrorCallback ::
                               (MonadDOM m) =>
                                 (Maybe SQLError -> JSM ()) -> m SQLTransactionErrorCallback
newSQLTransactionErrorCallback callback
  = liftDOM
      (SQLTransactionErrorCallback . Callback <$>
         function ""
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SQLTransactionErrorCallback Mozilla SQLTransactionErrorCallback documentation> 
newSQLTransactionErrorCallbackSync ::
                                   (MonadDOM m) =>
                                     (Maybe SQLError -> JSM ()) -> m SQLTransactionErrorCallback
newSQLTransactionErrorCallbackSync callback
  = liftDOM
      (SQLTransactionErrorCallback . Callback <$>
         function ""
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))

-- | <https://developer.mozilla.org/en-US/docs/Web/API/SQLTransactionErrorCallback Mozilla SQLTransactionErrorCallback documentation> 
newSQLTransactionErrorCallbackAsync ::
                                    (MonadDOM m) =>
                                      (Maybe SQLError -> JSM ()) -> m SQLTransactionErrorCallback
newSQLTransactionErrorCallbackAsync callback
  = liftDOM
      (SQLTransactionErrorCallback . Callback <$>
         function ""
           (\ _ _ [error] -> fromJSVal error >>= \ error' -> callback error'))