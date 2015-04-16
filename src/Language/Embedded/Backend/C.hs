{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generate C code from @Language.Embedded.Imperative@ programs
module Language.Embedded.Backend.C where

import Data.Proxy
import Control.Applicative
import Control.Monad.Operational.Compositional
import Language.Embedded.Imperative
import Language.C.Monad
import Language.C.Quote.C
import qualified Language.C.Syntax as C

-- | Identifiers from references
instance ToIdent (Ref a)
  where
    toIdent (RefComp r) = C.Id $ 'v':show r

-- | Compile `RefCMD`
compRefCMD :: forall exp prog a. CompExp exp
           => RefCMD (VarPred exp) exp prog a -> CGen a
compRefCMD cmd@NewRef = do
    t <- compTypePP2 (Proxy :: Proxy exp) cmd
    r <- RefComp <$> freshId
    addLocal [cdecl| $ty:t $id:r; |]
    return r
compRefCMD (InitRef exp) = do
    t <- compType exp
    r <- RefComp <$> freshId
    v   <- compExp exp
    addLocal [cdecl| $ty:t $id:r; |]
    addStm   [cstm| $id:r = $v; |]
    return r
compRefCMD cmd@(GetRef ref) = do
    t <- compTypeP cmd
    v <- varExp <$> freshId
    e <- compExp v
    addLocal [cdecl| $ty:t $id:ref; |]
    addStm   [cstm| $e = $id:ref; |]
    return v
compRefCMD (SetRef ref exp) = do
    v <- compExp exp
    addStm [cstm| $id:ref = $v; |]

-- | Identifiers from arrays
instance ToIdent (Arr i a)
  where
    toIdent (ArrComp arr) = C.Id arr

-- | Compile `ArrCMD`
compArrCMD :: forall exp prog a. CompExp exp
           => ArrCMD (VarPred exp) exp prog a -> CGen a
compArrCMD (NewArr size ini) = do
    addInclude "<string.h>"
    sym <- gensym "a"
    v   <- compExp size
    i   <- compExp ini
    t   <- compType ini
    addLocal [cdecl| $ty:t $id:sym[ $v ]; |]
    addStm   [cstm| memset($id:sym, $i, sizeof( $id:sym )); |]
    return $ ArrComp sym
-- compArrCMD (NewArr size init) = do
--     addInclude "<string.h>"
--     sym <- gensym "a"
--     v   <- compExp size
--     i   <- compExp init
--     addLocal [cdecl| float* $id:sym = calloc($v, sizeof(float)); |] -- todo: get real type
--     addFinalStm [cstm| free($id:sym); |]
--     addInclude "<stdlib.h>"
--     return $ ArrComp sym
compArrCMD (GetArr expi arr) = do
    v <- freshId
    let sym = 'v': show v
    i <- compExp expi
    t <- compTypePP (Proxy :: Proxy exp) arr
    addLocal [cdecl| $ty:t $id:sym; |]
    addStm   [cstm| $id:sym = $id:arr[ $i ]; |]
    return $ varExp v
compArrCMD (SetArr expi expv arr) = do
    v <- compExp expv
    i <- compExp expi
    addStm [cstm| $id:arr[ $i ] = $v; |]

-- | Compile `ControlCMD`
compControlCMD :: CompExp exp => ControlCMD exp CGen a -> CGen a
compControlCMD (If c t f) = do
    cc <- compExp c
    ct <- inNewBlock_ t
    cf <- inNewBlock_ f
    case (ct, cf) of
      ([],[]) -> return ()
      (_ ,[]) -> addStm [cstm| if (   $cc) {$items:ct} |]
      ([],_ ) -> addStm [cstm| if ( ! $cc) {$items:cf} |]
      (_ ,_ ) -> addStm [cstm| if (   $cc) {$items:ct} else {$items:cf} |]
compControlCMD (While cont body) = do
    bodyc <- inNewBlock_ $ do
        conte <- cont
        contc <- compExp conte
        case contc of
          C.Var (C.Id "true" _) _ -> return ()
          _ -> addStm [cstm| if (! $contc) {break;} |]
        body
    addStm [cstm| while (1) {$items:bodyc} |]
      -- TODO The b program should be re-executed at the end of each iteration
compControlCMD Break = addStm [cstm| break; |]

-- | Compile `FileCMD`
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
    i <- freshId
    let sym = 'v':show i
    addLocal [cdecl| float $id:sym; |]
    addStm   [cstm| fscanf($id:h, "%f", &$id:sym); |]
    return $ varExp i
compFileCMD (Eof (HandleComp h)) = do
    addInclude "<stdbool.h>"
    i <- freshId
    let sym = 'v':show i
    addLocal [cdecl| int $id:sym; |]
    addStm   [cstm| $id:sym = feof($id:h); |]
    return $ varExp i

-- | Compile `ConsoleCMD`
compConsoleCMD :: CompExp exp => ConsoleCMD exp CGen a -> CGen a
compConsoleCMD (Printf format a) = do
    addInclude "<stdio.h>"
    let format' = show format
    a' <- compExp a
    addStm [cstm| printf($id:format', $exp:a'); |]

-- | Generate a time sampling function
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

-- | Compile `TimeCMD`
compTimeCMD :: (CompExp exp, VarPred exp Double) => TimeCMD exp CGen a -> CGen a
compTimeCMD GetTime = do
    addInclude "<sys/time.h>"
    addInclude "<sys/resource.h>"
    addGlobal getTimeDef
    i <- freshId
    let sym = 't': show i
    addLocal [cdecl| double $id:sym; |]
    addStm   [cstm| $id:sym = get_time(); |]
    return $ varExp i

instance (CompExp exp, pred ~ (VarPred exp))  => Interp (RefCMD pred exp) CGen where interp = compRefCMD
instance (CompExp exp, pred ~ (VarPred exp))  => Interp (ArrCMD pred exp) CGen where interp = compArrCMD
instance CompExp exp                          => Interp (ControlCMD exp)  CGen where interp = compControlCMD
instance (CompExp exp, VarPred exp Bool, VarPred exp Float) => Interp (FileCMD exp)     CGen where interp = compFileCMD
instance CompExp exp                                        => Interp (ConsoleCMD exp)  CGen where interp = compConsoleCMD
instance (CompExp exp, VarPred exp Double)                  => Interp (TimeCMD exp)     CGen where interp = compTimeCMD



