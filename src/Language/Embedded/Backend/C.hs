{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Embedded.Backend.C where

import Data.Typeable
import Control.Monad.Operational.Compositional
import Language.Embedded.Imperative
import Language.C.Monad
import Language.C.Quote.C
import qualified Language.C.Syntax as C

----------------------------------------------------------------------------------------------------
-- * Compiling commands
----------------------------------------------------------------------------------------------------

compTypeRep :: TypeRep -> C.Type
compTypeRep trep = case show trep of
    "Bool"  -> [cty| int   |]
    "Int"   -> [cty| int   |]  -- todo: should only use fix-width Haskell ints
    "Float" -> [cty| float |]

typeOfP1 :: forall proxy a . Typeable a => proxy a -> TypeRep
typeOfP1 _ = typeOf (undefined :: a)

typeOfP2 :: forall proxy1 proxy2 a . Typeable a => proxy1 (proxy2 a) -> TypeRep
typeOfP2 _ = typeOf (undefined :: a)

compRefCMD :: CompExp exp => RefCMD (Typeable :/\: VarPred exp) exp prog a -> CGen a
compRefCMD cmd@NewRef = do
    let t = compTypeRep (typeOfP2 cmd)
    sym <- gensym "r"
    addLocal [cdecl| $ty:t $id:sym; |]
    return $ RefComp sym
compRefCMD cmd@(InitRef exp) = do
    let t = compTypeRep (typeOfP2 cmd)
    sym <- gensym "r"
    v   <- compExp exp
    addLocal [cdecl| $ty:t $id:sym; |]
    addStm   [cstm| $id:sym = $v; |]
    return $ RefComp sym
compRefCMD cmd@(GetRef (RefComp ref)) = do
    let t = compTypeRep (typeOfP2 cmd)
    sym <- gensym "r"
    addLocal [cdecl| $ty:t $id:sym; |]
    addStm   [cstm| $id:sym = $id:ref; |]
    return $ varExp sym
compRefCMD (SetRef (RefComp ref) exp) = do
    v <- compExp exp
    addStm [cstm| $id:ref = $v; |]
compRefCMD (UnsafeFreezeRef (RefComp ref)) = return $ varExp ref

compArrCMD :: CompExp exp => ArrCMD (Typeable :/\: VarPred exp) exp prog a -> CGen a
compArrCMD (NewArr size init) = do
    addInclude "<string.h>"
    sym <- gensym "a"
    v   <- compExp size
    i   <- compExp init -- todo: use this with memset
    addLocal [cdecl| float $id:sym[ $v ]; |] -- todo: get real type
    addStm   [cstm| memset($id:sym, $i, sizeof( $id:sym )); |]
    return $ ArrComp sym
-- compArrCMD (NewArr size init) = do
--     addInclude "<string.h>"
--     sym <- gensym "a"
--     v   <- compExp size
--     i   <- compExp init -- todo: use this with memset
--     addLocal [cdecl| float* $id:sym = calloc($v, sizeof(float)); |] -- todo: get real type
--     addFinalStm [cstm| free($id:sym); |]
--     addInclude "<stdlib.h>"
--     return $ ArrComp sym
compArrCMD (GetArr expi (ArrComp arr)) = do
    sym <- gensym "a"
    i   <- compExp expi
    addLocal [cdecl| float $id:sym; |] -- todo: get real type
    addStm   [cstm| $id:sym = $id:arr[ $i ]; |]
    return $ varExp sym
compArrCMD (SetArr expi expv (ArrComp arr)) = do
    v <- compExp expv
    i <- compExp expi
    addStm [cstm| $id:arr[ $i ] = $v; |]

compControlCMD :: CompExp exp => ControlCMD exp CGen a -> CGen a
compControlCMD (If c t f) = do
    cc <- compExp c
    ct <- inNewBlock_ t
    cf <- inNewBlock_ f
    case null cf of
      True  -> addStm [cstm| if ($cc) {$items:ct} |]
      False -> addStm [cstm| if ($cc) {$items:ct} else {$items:cf} |]
compControlCMD (While cont body) = do
    bodyc <- inNewBlock_ $ do
        conte <- cont
        contc <- compExp conte
        addStm [cstm| if (! $contc) {break;} |]
        body
    addStm [cstm| while (1) {$items:bodyc} |]
      -- TODO The b program should be re-executed at the end of each iteration
compControlCMD Break = addStm [cstm| break; |]

compFileCMD :: (CompExp exp, VarPred exp Bool, VarPred exp Float) => FileCMD exp CGen a -> CGen a
compFileCMD (Open path) = do
    addInclude "<stdio.h>"
    addInclude "<stdlib.h>"
    sym <- gensym "v"
    addLocal [cdecl| typename FILE * $id:sym; |]
    addStm   [cstm| $id:sym = fopen($id:path', "r+"); |]
    return $ HandleComp sym
  where
    path' = show path
compFileCMD (Close (HandleComp h)) = do
    addStm [cstm| fclose($id:h); |]
compFileCMD (Put (HandleComp h) exp) = do
    v <- compExp exp
    addStm [cstm| fprintf($id:h, "%f ", $v); |]
compFileCMD (Get (HandleComp h)) = do
    sym <- gensym "v"
    addLocal [cdecl| float $id:sym; |]
    addStm   [cstm| fscanf($id:h, "%f", &$id:sym); |]
    return $ varExp sym
compFileCMD (Eof (HandleComp h)) = do
    addInclude "<stdbool.h>"
    sym <- gensym "v"
    addLocal [cdecl| int $id:sym; |]
    addStm   [cstm| $id:sym = feof($id:h); |]
    return $ varExp sym

compConsoleCMD :: CompExp exp => ConsoleCMD exp CGen a -> CGen a
compConsoleCMD (Printf format a) = do
    addInclude "<stdio.h>"
    let format' = show format
    a' <- compExp a
    addStm [cstm| printf($id:format', $exp:a'); |]

getTimeDef :: C.Definition
getTimeDef = [cedecl|
// From http://stackoverflow.com/questions/2349776/how-can-i-benchmark-c-code-easily
double get_time()
{
    struct timeval t;
    struct timezone tzp;
    gettimeofday(&t, &tzp);
    return t.tv_sec + t.tv_usec*1e-6;
}
|]

compTimeCMD :: (CompExp exp, VarPred exp Double) => TimeCMD exp CGen a -> CGen a
compTimeCMD GetTime = do
    addInclude "<sys/time.h>"
    addInclude "<sys/resource.h>"
    addGlobal getTimeDef
    sym <- gensym "t"
    addLocal [cdecl| double $id:sym; |]
    addStm   [cstm| $id:sym = get_time(); |]
    return $ varExp sym

instance (CompExp exp, pred ~ (Typeable :/\: VarPred exp))  => Interp (RefCMD pred exp) CGen where interp = compRefCMD
instance (CompExp exp, pred ~ (Typeable :/\: VarPred exp))  => Interp (ArrCMD pred exp) CGen where interp = compArrCMD
instance CompExp exp                                        => Interp (ControlCMD exp)  CGen where interp = compControlCMD
instance (CompExp exp, VarPred exp Bool, VarPred exp Float) => Interp (FileCMD exp)     CGen where interp = compFileCMD
instance CompExp exp                                        => Interp (ConsoleCMD exp)  CGen where interp = compConsoleCMD
instance (CompExp exp, VarPred exp Double)                  => Interp (TimeCMD exp)     CGen where interp = compTimeCMD



