{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}



module Scrappy.JS where

import Prelude
import Prelude (IO(..))
import GHC.IO (IO(IO))
import GHC.Exts
import System.IO.Unsafe (unsafeInterleaveIO)

import Scrappy.Requests (getHtml, SessionState(..))
import Scrappy.Scrape (scrape)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Elem.Types (Elem'(..))


-- import Language.Javascript.JSaddle.Run (wrapJSVal')
import Language.Javascript.JSaddle (runJSaddle, eval, showJSValue, valToText)
import Language.Javascript.JSaddle.Exception (JSException(..))
import Language.Javascript.JSaddle.Types (JSVal(..), Object(..), Result(..), JSContextRef(..), JSValueReceived(..)
                                         , AsyncCommand(..), JSValueForSend(..))
import Control.Exception (throwIO, evaluate)
import Control.Concurrent.STM (atomically, writeTChan, newTChanIO)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import Control.Concurrent.MVar (putMVar, modifyMVar, takeMVar, newEmptyMVar, newMVar)
import Data.IORef (atomicWriteIORef, readIORef, mkWeakIORef, newIORef)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)

import Data.Maybe (fromJust )
import GHC.Conc (ThreadId(..), myThreadId)
import Control.DeepSeq (deepseq)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text (Text, pack)
import System.Random (randomIO)
import Data.Time.Clock (getCurrentTime)


-- Scrape all script elems
-- Fetch if src attribute not null and set HTTP body to the key of the data struct
-- Append all script bodies together

-- 1. scrape (el "script" []) html


type Body = Text
type Attributes = M.Map String String
data Script = Script Attributes Body deriving (Show, Eq) 

type Html = Text

instance Semigroup Script where
  (Script _ b) <> (Script _ b2) = (Script mempty (b <> b2))

instance Monoid Script where
  mempty = Script mempty mempty 


-- Can be 'a' cuz its discarded
toScript :: SessionState sv => sv -> Elem' a -> IO Script
toScript mgr (Elem' _ atrs _ inner) = do
  let src = M.lookup "src" atrs 
  case (null inner && (src /= Nothing)) of
    True -> do
      (scriptBody, _) <- getHtmlST mgr $ fromJust src
      pure $ Script atrs $ pack scriptBody 
    False ->
      pure $ Script atrs $ pack inner

-- getHtmlRaw = getHtmlST

getHtmlFinal :: SessionState sv => sv -> String -> IO Text
getHtmlFinal mgr url = do
  (html, _) <- getHtmlST mgr url
  -- print html
  let
    Just scriptElems = scrape (el "script" []) html
    -- scripts' = scripts >>= (\ss -> fmap toScript ss)
  scripts <- mapM (toScript mgr) scriptElems
  let
    Script _ firstScript = head scripts
    Script _ theScript = mconcat scripts 
  runScript firstScript--theScript
  
  
-- data HTML = Raw Html | TreeH (Forest HTML) -- theoretically speaking  

-- TODO(galen): change return document to just being done in the JSM monad 
runScript :: Text -> IO Text
runScript script = do
  ctxRef <- mkContextRef
  runJSaddle ctxRef $ valToText =<< (eval $ finallyGetDocument script)
  where
    finallyGetDocument :: Text -> Text
    finallyGetDocument script = script <> "; return document;"


--- TAKEN FROM JSaddle run



-- runJavaScript :: (Batch -> IO ()) -> JSM () -> IO (Results -> IO (), Results -> IO Batch, IO ())
-- runJavaScript sendBatch entryPoint = do

mkContextRef :: IO JSContextRef
mkContextRef = do 
    contextId' <- randomIO
    startTime' <- getCurrentTime
    recvMVar <- newEmptyMVar
    lastAsyncBatch <- newEmptyMVar
    commandChan <- newTChanIO
    callbacks <- newTVarIO M.empty
    nextRef' <- newTVarIO 0
    finalizerThreads' <- newMVar S.empty
    animationFrameHandlers' <- newMVar []
    loggingEnabled <- newIORef False
    liveRefs' <- newMVar S.empty
    let ctx = JSContextRef {
          contextId = contextId'
          , startTime = startTime'
          , doSendCommand = \cmd -> cmd `deepseq` do
              result <- newEmptyMVar
              atomically $ writeTChan commandChan (Right (cmd, result))
              unsafeInterleaveIO $
                takeMVar result >>= \case
                (ThrowJSValue v) -> do
                  jsval <- wrapJSVal' ctx v
                  throwIO $ JSException jsval
                r -> return r
          , doSendAsyncCommand = \cmd -> cmd `deepseq` atomically (writeTChan commandChan $ Left cmd)
          , addCallback = \(Object (JSVal ioref)) cb -> do
              val <- readIORef ioref
              atomically $ modifyTVar' callbacks (M.insert val cb)
          , nextRef = nextRef'
          , doEnableLogging = atomicWriteIORef loggingEnabled
          , finalizerThreads = finalizerThreads'
          , animationFrameHandlers = animationFrameHandlers'
          , liveRefs = liveRefs'
          }
    pure ctx 



wrapJSVal' :: JSContextRef -> JSValueReceived -> IO JSVal
wrapJSVal' ctx (JSValueReceived n) = do
    ref <- liftIO $ newIORef n
    when (n >= 5 || n < 0) $
-- #ifdef JSADDLE_CHECK_WRAPJSVAL
     do lr <- takeMVar $ liveRefs ctx
        if n `S.member` lr
            then do
                putStrLn $ "JS Value Ref " <> show n <> " already wrapped"
                putMVar (liveRefs ctx) lr
            else putMVar (liveRefs ctx) =<< evaluate (S.insert n lr)
-- #endif
        void . mkWeakIORef ref $ do
            ft <- takeMVar $ finalizerThreads ctx
            t <- myThreadId
            let tname = pack $ show t
            doSendAsyncCommand ctx $ FreeRef tname $ JSValueForSend n
            if tname `S.member` ft
                then putMVar (finalizerThreads ctx) ft
                else do
                    addThreadFinalizer t $ do
                        modifyMVar (finalizerThreads ctx) $ \s -> return (S.delete tname s, ())
                        doSendAsyncCommand ctx $ FreeRefs tname
                    putMVar (finalizerThreads ctx) =<< evaluate (S.insert tname ft)
    return (JSVal ref)



addThreadFinalizer :: ThreadId -> IO () -> IO ()
addThreadFinalizer t@(ThreadId t#) (IO finalizer) =
    IO $ \s -> case mkWeak# t# t finalizer s of { (# s1, _ #) -> (# s1, () #) }
