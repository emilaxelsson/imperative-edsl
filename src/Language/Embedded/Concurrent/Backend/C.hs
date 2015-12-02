{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Embedded.Concurrent.Backend.C where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Operational.Higher
import Data.Proxy
import Language.Embedded.Expression
import Language.Embedded.Concurrent.CMD
import Language.C.Quote.C
import Language.C.Monad
import qualified Language.C.Syntax as C



instance ToIdent ThreadId where
  toIdent (TIDComp tid) = C.Id $ "t" ++ show tid

instance ToIdent (Chan t a) where
  toIdent (ChanComp c) = C.Id $ "chan" ++ show c

threadFun :: ThreadId -> String
threadFun tid = "thread_" ++ show tid

-- | Compile `ThreadCMD`.
--   TODO: sharing for threads with the same body
compThreadCMD :: ThreadCMD CGen a -> CGen a
compThreadCMD (ForkWithId body) = do
  tid <- TIDComp <$> freshId
  let funName = threadFun tid
  _ <- inFunctionTy [cty|void*|] funName $ do
    addParam [cparam| void* unused |]
    body tid
    addStm [cstm| return NULL; |]
  addSystemInclude "pthread.h"
  touchVar tid
  addLocal [cdecl| typename pthread_t $id:tid; |]
  addStm [cstm| pthread_create(&$id:tid, NULL, $id:funName, NULL); |]
  return tid
compThreadCMD (Kill tid) = do
  touchVar tid
  addStm [cstm| pthread_cancel($id:tid); |]
compThreadCMD (Wait tid) = do
  touchVar tid
  addStm [cstm| pthread_join($id:tid, NULL); |]

-- | Compile `ChanCMD`.
compChanCMD :: forall exp prog a. CompExp exp
            => ChanCMD exp prog a
            -> CGen a
compChanCMD cmd@(NewChan sz) = do
  addLocalInclude "chan.h"
  t <- compTypePP2 (Proxy :: Proxy exp) cmd
  sz' <- compExp sz
  c <- ChanComp <$> freshId
  addGlobal [cedecl| typename chan_t $id:c; |]
  addStm [cstm| $id:c = chan_new(sizeof($ty:t), $sz'); |]
  return c
compChanCMD (WriteChan c x) = do
  x' <- compExp x
  (v,name) <- freshVar
  (ok,okname) <- freshVar
  let _ = v `asTypeOf` x
  addStm [cstm| $id:name = $x'; |]
  addStm [cstm| $id:okname = chan_write($id:c, &$id:name); |]
  return ok
compChanCMD (ReadChan c) = do
  (var,name) <- freshVar
  addStm [cstm| chan_read($id:c, &$id:name); |]
  return var
compChanCMD (CloseChan c) = do
  addStm [cstm| chan_close($id:c); |]
compChanCMD (ReadOK c) = do
  (var,name) <- freshVar
  addStm [cstm| $id:name = chan_last_read_ok($id:c); |]
  return var

instance Interp ThreadCMD CGen where
  interp = compThreadCMD
instance CompExp exp => Interp (ChanCMD exp) CGen where
  interp = compChanCMD

