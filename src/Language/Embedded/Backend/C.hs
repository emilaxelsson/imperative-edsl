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

freshVar :: forall exp a. (CompExp exp, VarPred exp a) => CGen (exp a, C.Id)
freshVar = do
    v <- varExp <$> freshId
    t <- compTypeP (Proxy :: Proxy (exp a))
    C.Var n _ <- compExp v
    touchVar n
    addLocal [cdecl| $ty:t $id:n; |]
    return (v,n)

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
    v <- compExp exp
    addLocal [cdecl| $ty:t $id:r; |]
    addStm   [cstm| $id:r = $v; |]
    return r
compRefCMD (GetRef ref) = do
    (v,_) <- freshVar
    e <- compExp v
    addStm [cstm| $e = $id:ref; |]
    return v
compRefCMD (SetRef ref exp) = do
    v <- compExp exp
    touchVar ref
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
compArrCMD (GetArr expi arr) = do
    (v,n) <- freshVar
    i     <- compExp expi
    touchVar arr
    addStm [cstm| $id:n = $id:arr[ $i ]; |]
    return v
compArrCMD (SetArr expi expv arr) = do
    v <- compExp expv
    i <- compExp expi
    touchVar arr
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
compControlCMD Break = addStm [cstm| break; |]

compIOMode :: IOMode -> String
compIOMode ReadMode      = "r"
compIOMode WriteMode     = "w"
compIOMode AppendMode    = "a"
compIOMode ReadWriteMode = "r+"

-- | Compile `FileCMD`
compFileCMD :: (CompExp exp, VarPred exp Bool) => FileCMD exp CGen a -> CGen a
compFileCMD (FOpen path mode) = do
    addInclude "<stdio.h>"
    addInclude "<stdlib.h>"
    sym <- gensym "v"
    addLocal [cdecl| typename FILE * $id:sym; |]
    addStm   [cstm| $id:sym = fopen($id:path',$string:mode'); |]
    return $ HandleComp sym
  where
    path' = show path
    mode' = compIOMode mode
compFileCMD (FClose (HandleComp h)) = do
    touchVar h
    addStm [cstm| fclose($id:h); |]
compFileCMD (FPrintf (HandleComp h) form as) = do
    addInclude "<stdio.h>"
    touchVar h
    let h'    = [cexp| $id:h |]
        form' = [cexp| $id:form |]
    as' <- fmap ([h',form']++) $ sequence [compExp a | FunArg a <- as]
    addStm [cstm| fprintf($args:as'); |]
compFileCMD cmd@(FGet (HandleComp h)) = do
    (v,n) <- freshVar
    touchVar h
    let mkProxy :: FileCMD exp prog (exp a) -> Proxy a
        mkProxy _ = Proxy
        form = scanFormatSpecifier (mkProxy cmd)
    addStm [cstm| fscanf($id:h, $string:form, &$id:n); |]
    return v
compFileCMD (FEof (HandleComp h)) = do
    addInclude "<stdbool.h>"
    (v,n) <- freshVar
    touchVar h
    addStm [cstm| $id:n = feof($id:h); |]
    return v

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

instance (CompExp exp, pred ~ (VarPred exp)) => Interp (RefCMD pred exp) CGen where interp = compRefCMD
instance (CompExp exp, pred ~ (VarPred exp)) => Interp (ArrCMD pred exp) CGen where interp = compArrCMD
instance CompExp exp                         => Interp (ControlCMD exp)  CGen where interp = compControlCMD
instance (CompExp exp, VarPred exp Bool)     => Interp (FileCMD exp)     CGen where interp = compFileCMD
instance (CompExp exp, VarPred exp Double)   => Interp (TimeCMD exp)     CGen where interp = compTimeCMD

