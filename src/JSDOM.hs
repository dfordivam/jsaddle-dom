{-# LANGUAGE CPP, OverloadedStrings, PatternSynonyms #-}
#ifndef ghcjs_HOST_OS
{-# LANGUAGE RecursiveDo #-}
#endif
module JSDOM (
  currentWindow
, currentWindowUnchecked
, currentDocument
, currentDocumentUnchecked
, AnimationFrameHandle
, inAnimationFrame
, inAnimationFrame'
) where

#ifdef ghcjs_HOST_OS
import JSDOM.Types
       (FromJSVal(..), MonadDOM, liftDOM, Document(..), Window(..), JSM)
import Language.Javascript.JSaddle.Object (jsg)
import JavaScript.Web.AnimationFrame (AnimationFrameHandle, inAnimationFrame)
#else
import Control.Monad (void, forM_, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, swapMVar)
import Language.Javascript.JSaddle.Types (JSContextRef(..), askJSM)
import Language.Javascript.JSaddle.Object (freeFunction, jsg)
import JSDOM.Types
       (Callback(..), RequestAnimationFrameCallback(..), FromJSVal(..),
        MonadDOM, liftDOM, Document(..), Window(..), JSM, JSContextRef(..))
import JSDOM.Generated.RequestAnimationFrameCallback
       (newRequestAnimationFrameCallbackSync)
import JSDOM.Generated.Window (requestAnimationFrame)
import System.IO.Unsafe (unsafePerformIO)
#endif
import GHCJS.Concurrent (OnBlocked(..))

currentWindow :: MonadDOM m => m (Maybe Window)
currentWindow = liftDOM $ jsg ("window" :: String) >>= fromJSVal

currentWindowUnchecked :: MonadDOM m => m Window
currentWindowUnchecked = liftDOM $ jsg ("window" :: String) >>= fromJSValUnchecked

currentDocument :: MonadDOM m => m (Maybe Document)
currentDocument = liftDOM $ jsg ("document" :: String) >>= fromJSVal

currentDocumentUnchecked :: MonadDOM m => m Document
currentDocumentUnchecked = liftDOM $ jsg ("document" :: String) >>= fromJSValUnchecked

#ifndef ghcjs_HOST_OS

data AnimationFrameHandle = AnimationFrameHandle

animationFrameHandlerVar :: MVar [Double -> JSM ()]
animationFrameHandlerVar = unsafePerformIO $ newMVar []
{-# NOINLINE animationFrameHandlerVar #-}

{- |
     Run the action in an animationframe callback. The action runs in a
     synchronous thread, and is passed the high-performance clock time
     stamp for that frame.
 -}
inAnimationFrame :: OnBlocked       -- ^ what to do when encountering a blocking call
                 -> (Double -> JSM ())  -- ^ the action to run
                 -> JSM AnimationFrameHandle
inAnimationFrame _ f = do
    -- Add this handler to the list to be run by the callback
    -- It is important to do sync here, otherwise we may end up having race
    -- liftIO does a sync internally
    wasEmpty <- liftIO $ modifyMVar animationFrameHandlerVar $ \old ->
      pure (f : old, null old)
    -- If this was the first handler added set up a callback
    -- to run the handlers in the next animation frame.
    when wasEmpty $ do
        win <- currentWindowUnchecked
        rec cb@(RequestAnimationFrameCallback (Callback fCb)) <- newRequestAnimationFrameCallbackSync $ \t -> do
              -- This is a one off handler so free it when it runs
              freeFunction fCb
              -- Take the list of handers and empty it
              handlersToRun <- liftIO $ swapMVar animationFrameHandlerVar []
              -- Exectute handlers in the order
              forM_ (reverse handlersToRun) (\handler -> handler t)
        -- Add the callback function
        void $ requestAnimationFrame win cb
    return AnimationFrameHandle

#endif

{- |
     Run the action in an animationframe callback. The action runs in a
     synchronous thread, and is passed the high-performance clock time
     stamp for that frame.  On GHCJS this version will continue
     asynchronously if it is not possible to complete the callback
     synchronously.
 -}
inAnimationFrame' :: (Double -> JSM ())  -- ^ the action to run
                 -> JSM AnimationFrameHandle
inAnimationFrame' = inAnimationFrame ContinueAsync
