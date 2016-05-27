{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Embedded.Concurrent.Backend.C where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Operational.Higher
import Data.Typeable
import Language.Embedded.Expression
import Language.Embedded.Concurrent.CMD
import Language.Embedded.Imperative.CMD
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
compThreadCMD :: CompExp exp => ThreadCMD (Param3 CGen exp pred) a -> CGen a
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
compThreadCMD (Sleep us) = do
  us' <- compExp us
  addSystemInclude "unistd.h"
  addStm [cstm| usleep($us'); |]

-- | Compile `ChanCMD`.
compChanCMD :: (CompExp exp, CompTypeClass ct, ct Bool)
            => ChanCMD (Param3 CGen exp ct) a
            -> CGen a
compChanCMD cmd@(NewChan sz) = do
  addLocalInclude "chan.h"
  sz' <-compChanSize sz
  c <- ChanComp <$> gensym "chan"
  addGlobal [cedecl| typename chan_t $id:c; |]
  addStm [cstm| $id:c = chan_new($sz'); |]
  return c
compChanCMD cmd@(WriteOne c (x :: exp a)) = do
  x'         <- compExp x
  v :: Val a <- freshVar (proxyPred cmd)
  ok         <- freshVar (proxyPred cmd)
  addStm [cstm| $id:v = $x'; |]
  addStm [cstm| $id:ok = chan_write($id:c, sizeof($id:v), &$id:v); |]
  return ok
compChanCMD cmd@(WriteChan c from to (ArrComp arr)) = do
  from' <- compExp from
  to' <- compExp to
  ok <- freshVar (proxyPred cmd)
  addStm [cstm| $id:ok = chan_write($id:c, sizeof(*$id:arr)*(($to')-($from')), &$id:arr[$from']); |]
  return ok
compChanCMD cmd@(ReadOne c) = do
  v <- freshVar (proxyPred cmd)
  addStm [cstm| chan_read($id:c, sizeof($id:v), &$id:v); |]
  return v
compChanCMD cmd@(ReadChan c from to (ArrComp arr)) = do
  ok <- freshVar (proxyPred cmd)
  from' <- compExp from
  to' <- compExp to
  addStm [cstm| chan_read($id:c, sizeof(*$id:arr)*(($to')-($from')), &$id:arr[$from']); |]
  addStm [cstm| $id:ok = chan_last_read_ok($id:c); |]
  return ok
compChanCMD (CloseChan c) = do
  addStm [cstm| chan_close($id:c); |]
compChanCMD cmd@(ReadOK c) = do
  var <- freshVar (proxyPred cmd)
  addStm [cstm| $id:var = chan_last_read_ok($id:c); |]
  return var

compChanSize :: forall exp ct i. (CompExp exp, CompTypeClass ct) => ChanSize exp ct i -> CGen C.Exp
compChanSize (OneSize t sz) = do
  t' <- compType (Proxy :: Proxy ct) t
  sz' <- compExp sz
  return [cexp| $sz' * sizeof($ty:t') |]
compChanSize (TimesSize n sz) = do
  n' <- compExp n
  sz' <- compChanSize sz
  return [cexp| $n' * $sz' |]
compChanSize (PlusSize a b) = do
  a' <- compChanSize a
  b' <- compChanSize b
  return [cexp| $a' + $b' |]

instance CompExp exp => Interp ThreadCMD CGen (Param2 exp pred) where
  interp = compThreadCMD
instance (CompExp exp, CompTypeClass ct, ct Bool) => Interp ChanCMD CGen (Param2 exp ct) where
  interp = compChanCMD
