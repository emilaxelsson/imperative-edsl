{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Embedded.Concurrent.Backend.C where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Operational.Higher
import Language.Embedded.Expression
import Language.Embedded.Concurrent.CMD
import Language.Embedded.Backend.C.Expression
import Language.C.Quote.C
import Language.C.Monad
import qualified Language.C.Syntax as C



instance ToIdent ThreadId where
  toIdent (TIDComp tid) = C.Id tid

instance ToIdent (Chan t a) where
  toIdent (ChanComp c) = C.Id c

threadFun :: ThreadId -> String
threadFun tid = "thread_" ++ show tid

-- | Compile `ThreadCMD`.
--   TODO: sharing for threads with the same body
compThreadCMD :: ThreadCMD (Param3 CGen exp pred) a -> CGen a
compThreadCMD (ForkWithId body) = do
  tid <- TIDComp <$> gensym "t"
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
compChanCMD :: (CompExp exp, CompTypeClass ct, ct Bool)
            => ChanCMD (Param3 CGen exp ct) a
            -> CGen a
compChanCMD cmd@(NewChan sz) = do
  addLocalInclude "chan.h"
  t <- compType (proxyPred cmd) (proxyArg cmd)
  sz' <- compExp sz
  c <- ChanComp <$> gensym "chan"
  addGlobal [cedecl| typename chan_t $id:c; |]
  addStm [cstm| $id:c = chan_new($sz'*sizeof($ty:t)); |]
  return c
compChanCMD cmd@(WriteChan c (x :: exp a)) = do
  x'         <- compExp x
  v :: Val a <- freshVar (proxyPred cmd)
  ok         <- freshVar (proxyPred cmd)
  addStm [cstm| $id:v = $x'; |]
  addStm [cstm| $id:ok = chan_write($id:c, sizeof($id:v), &$id:v); |]
  return ok
compChanCMD cmd@(ReadChan c) = do
  v <- freshVar (proxyPred cmd)
  addStm [cstm| chan_read($id:c, sizeof($id:v), &$id:v); |]
  return v
compChanCMD (CloseChan c) = do
  addStm [cstm| chan_close($id:c); |]
compChanCMD cmd@(ReadOK c) = do
  var <- freshVar (proxyPred cmd)
  addStm [cstm| $id:var = chan_last_read_ok($id:c); |]
  return var

instance Interp ThreadCMD CGen (Param2 exp pred) where
  interp = compThreadCMD
instance (CompExp exp, CompTypeClass ct, ct Bool) => Interp ChanCMD CGen (Param2 exp ct) where
  interp = compChanCMD

