{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | C code generation for imperative commands

module Language.Embedded.Imperative.Backend.C where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.State
import Data.Proxy

import Language.C.Quote.GCC
import qualified Language.C.Syntax as C

import Control.Monad.Operational.Higher
import Language.C.Monad
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Frontend.General
import Language.Embedded.Backend.C



-- | Compile `RefCMD`
compRefCMD :: (CompExp exp, CompTypeClass ct) =>
    RefCMD (Param3 prog exp ct) a -> CGen a
compRefCMD cmd@(NewRef base) = do
    t <- compType (proxyPred cmd) (proxyArg cmd)
    r <- RefComp <$> gensym base
    addLocal $ case t of
      C.Type _ C.Ptr{} _ -> [cdecl| $ty:t $id:r = NULL; |]
      _                  -> [cdecl| $ty:t $id:r; |]
    return r
compRefCMD cmd@(InitRef base exp) = do
    t <- compType (proxyPred cmd) exp
    r <- RefComp <$> gensym base
    e <- compExp exp
    addLocal [cdecl| $ty:t $id:r; |]
    addStm   [cstm| $id:r = $e; |]
    return r
compRefCMD cmd@(GetRef ref) = do
    v <- freshVar (proxyPred cmd)
    touchVar ref
    addStm [cstm| $id:v = $id:ref; |]
    return v
compRefCMD (SetRef ref exp) = do
    v <- compExp exp
    touchVar ref
    addStm [cstm| $id:ref = $v; |]
compRefCMD (UnsafeFreezeRef (RefComp v)) = return $ ValComp v

-- The `IsPointer` instance for `Arr` demands that arrays are represented as
-- pointers in C (because `IsPointer` enables use of `SwapPtr`). As explained
-- [here](http://stackoverflow.com/questions/3393518/swap-arrays-by-using-pointers-in-c),
-- arrays in C are *not* pointers in the sense that they can be redirected. Here
-- "arrays" means variables declared as e.g. `int arr[10];`. This is why we
-- declare a supplementary pointer for such arrays; e.g:
--
--     int _a[] = {0,1,2,3,4,5,6,7,8,9};
--     int * a = _a;
--
-- The declaration of a variable-sized array could only be done where its size
-- expression can be evaluated. This is why the declaration of variable-sized
-- arrays is done with `addItem` insted of `addLocal`: it preserves the position
-- of the declaration in the block, as it would be a statement.
--
-- Pointers that are used between multiple functions will be lifted to shared globals.
-- To ensure the correctness of the resulting program the underlying arrays must also
-- be lifted, hence the extra `touchVar` application on their symbols.

-- | Generates the symbol name as an identifier for a given array.
newtype BaseArrOf i a = BaseArrOf (Arr i a)

instance ToIdent (BaseArrOf i a)
  where toIdent (BaseArrOf (ArrComp sym)) = toIdent $ '_':sym

-- | Compile `ArrCMD`
compArrCMD :: (CompExp exp, CompTypeClass ct) =>
    ArrCMD (Param3 prog exp ct) a -> CGen a
compArrCMD cmd@(NewArr base align size) = do
    sym <- gensym base
    let sym' = '_':sym
    n <- compExp size
    t <- compType (proxyPred cmd) (proxyArg cmd)
    case n of
      C.Const _ _ -> do
        case align of
          Just a -> do
            let a' = fromIntegral a :: Int
            addLocal [cdecl| $ty:t $id:sym'[ $n ] __attribute__((aligned($a'))); |]
          _ -> addLocal [cdecl| $ty:t $id:sym'[ $n ]; |]
        addLocal [cdecl| $ty:t * $id:sym = $id:sym'; |]  -- explanation above
      _ -> do
        case align of
          Just a -> do
            let a' = fromIntegral a :: Int
            addItem [citem| $ty:t $id:sym'[ $n ] __attribute__((aligned($a'))); |]
          _ -> addItem [citem| $ty:t $id:sym'[ $n ]; |]
        addItem [citem| $ty:t * $id:sym = $id:sym'; |]  -- explanation above
    return $ ArrComp sym
compArrCMD cmd@(InitArr base align as) = do
    sym <- gensym base
    let sym' = '_':sym
    t   <- compType (proxyPred cmd) (proxyArg cmd)
    as' <- mapM (compLit (proxyPred cmd)) as
    case align of
      Just a -> do
        let a' = fromIntegral a :: Int
        addLocal [cdecl| $ty:t $id:sym'[] __attribute__((aligned($a'))) = $init:(arrayInit as'); |]
      _ -> addLocal [cdecl| $ty:t $id:sym'[] = $init:(arrayInit as');|]
    addLocal [cdecl| $ty:t * $id:sym = $id:sym'; |]  -- explanation above
    return $ ArrComp sym
compArrCMD cmd@(GetArr expi arr) = do
    v <- freshVar (proxyPred cmd)
    i <- compExp expi
    touchVar $ BaseArrOf arr  -- explanation above
    touchVar arr
    addStm [cstm| $id:v = $id:arr[ $i ]; |]
    return v
compArrCMD (SetArr expi expv arr) = do
    v <- compExp expv
    i <- compExp expi
    touchVar $ BaseArrOf arr  -- explanation above
    touchVar arr
    addStm [cstm| $id:arr[ $i ] = $v; |]
compArrCMD cmd@(CopyArr arr1 arr2 expl) = do
    addInclude "<string.h>"
    mapM_ touchVar [BaseArrOf arr1,BaseArrOf arr2]  -- explanation above
    mapM_ touchVar [arr1,arr2]
    l <- compExp expl
    t <- compType (proxyPred cmd) arr1
    addStm [cstm| memcpy($id:arr1, $id:arr2, $l * sizeof($ty:t)); |]
compArrCMD (UnsafeFreezeArr (ArrComp arr)) = return $ IArrComp arr
compArrCMD (UnsafeThawArr (IArrComp arr))  = return $ ArrComp arr


-- | Compile `ControlCMD`
compControlCMD :: (CompExp exp, CompTypeClass ct) =>
    ControlCMD (Param3 CGen exp ct) a -> CGen a
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
    s <- get
    noop <- do
        conte <- cont
        contc <- compExp conte
        case contc of
          C.Var (C.Id "false"  _) _ -> return True
          _ -> return False
    put s
    bodyc <- inNewBlock_ $ do
        conte <- cont
        contc <- compExp conte
        case contc of
          C.Var (C.Id "true"  _) _ -> return ()
          _ -> case viewNotExp contc of
              Just a -> addStm [cstm| if ($a) {break;} |]
              _      -> addStm [cstm| if (! $contc) {break;} |]
        body
    when (not noop) $ addStm [cstm| while (1) {$items:bodyc} |]
compControlCMD cmd@(For (lo,step,hi) body) = do
    loe <- compExp lo
    hie <- compExp $ borderVal hi
    i   <- freshVar (proxyPred cmd)
    bodyc <- inNewBlock_ (body i)
    let incl = borderIncl hi
    let conte
          | incl && (step>=0) = [cexp| $id:i<=$hie |]
          | incl && (step<0)  = [cexp| $id:i>=$hie |]
          | step >= 0         = [cexp| $id:i< $hie |]
          | step < 0          = [cexp| $id:i> $hie |]
    let stepe
          | step == 1    = [cexp| $id:i++ |]
          | step == (-1) = [cexp| $id:i-- |]
          | step == 0    = [cexp| 0 |]
          | step >  0    = [cexp| $id:i = $id:i + $step |]
          | step <  0    = [cexp| $id:i = $id:i - $(negate step) |]
    addStm [cstm| for ($id:i=$loe; $conte; $stepe) {$items:bodyc} |]
compControlCMD Break = addStm [cstm| break; |]
compControlCMD (Assert cond msg) = do
    addInclude "<assert.h>"
    c <- compExp cond
    addStm [cstm| assert($c && $msg); |]

compPtrCMD :: PtrCMD (Param3 prog exp pred) a -> CGen a
compPtrCMD (SwapPtr a b) = do
    sym <- gensym "tmp"
    addLocal [cdecl| void * $id:sym; |]
    addStm   [cstm| $id:sym = $id:a; |]
    addStm   [cstm| $id:a = $id:b; |]
    addStm   [cstm| $id:b = $id:sym; |]

compIOMode :: IOMode -> String
compIOMode ReadMode      = "r"
compIOMode WriteMode     = "w"
compIOMode AppendMode    = "a"
compIOMode ReadWriteMode = "r+"

-- | Compile `FileCMD`
compFileCMD :: (CompExp exp, CompTypeClass ct, ct Bool) =>
    FileCMD (Param3 prog exp ct) a -> CGen a
compFileCMD (FOpen path mode) = do
    addInclude "<stdio.h>"
    addInclude "<stdlib.h>"
    sym <- gensym "f"
    addLocal [cdecl| typename FILE * $id:sym; |]
    addStm   [cstm| $id:sym = fopen($id:path',$string:mode'); |]
    return $ HandleComp sym
  where
    path' = show path
    mode' = compIOMode mode
compFileCMD (FClose h) = do
    addInclude "<stdio.h>"
    touchVar h
    addStm [cstm| fclose($id:h); |]
compFileCMD (FPrintf h form as) = do
    addInclude "<stdio.h>"
    touchVar h
    let h'     = [cexp| $id:h |]
        form'  = show form
        form'' = [cexp| $id:form' |]
    as' <- fmap ([h',form'']++) $ sequence [compExp a | PrintfArg a <- as]
    addStm [cstm| fprintf($args:as'); |]
compFileCMD cmd@(FGet h) = do
    addInclude "<stdio.h>"
    v <- freshVar (proxyPred cmd)
    touchVar h
    let mkProxy = (\_ -> Proxy) :: FileCMD (Param3 prog exp pred) (Val a) -> Proxy a
        form    = formatSpecScan (mkProxy cmd)
    addStm [cstm| fscanf($id:h, $string:form, &$id:v); |]
    return v
compFileCMD cmd@(FEof h) = do
    addInclude "<stdbool.h>"
    addInclude "<stdio.h>"
    v <- freshVar (proxyPred cmd)
    touchVar h
    addStm [cstm| $id:v = feof($id:h); |]
    return v

compC_CMD :: (CompExp exp, CompTypeClass ct) =>
    C_CMD (Param3 CGen exp ct) a -> CGen a
compC_CMD cmd@(NewPtr base) = do
    addInclude "<stddef.h>"
    p <- PtrComp <$> gensym base
    t <- compType (proxyPred cmd) (proxyArg cmd)
    addLocal [cdecl| $ty:t * $id:p = NULL; |]
    return p
compC_CMD (PtrToArr (PtrComp p)) = return $ ArrComp p
compC_CMD (NewObject base t pointed) = do
    o <- Object pointed t <$> gensym base
    let t' = namedType t
    if pointed
      then addLocal [cdecl| $ty:t' * $id:o; |]
      else addLocal [cdecl| $ty:t' $id:o; |]
    return o
compC_CMD (AddInclude inc)    = addInclude inc
compC_CMD (AddDefinition def) = addGlobal def
compC_CMD cmd@(AddExternFun fun res args) = do
    tres  <- compType (proxyPred cmd) res
    targs <- mapM mkParam args
    addGlobal [cedecl| extern $ty:tres $id:fun($params:targs); |]
compC_CMD (AddExternProc proc args) = do
    targs <- mapM mkParam args
    addGlobal [cedecl| extern void $id:proc($params:targs); |]
compC_CMD cmd@(CallFun fun as) = do
    as' <- mapM mkArg as
    v   <- freshVar (proxyPred cmd)
    addStm [cstm| $id:v = $id:fun($args:as'); |]
    return v
compC_CMD (CallProc obj fun as) = do
    as' <- mapM mkArg as
    case obj of
      Nothing -> addStm [cstm| $id:fun($args:as'); |]
      Just o  -> addStm [cstm| $id:o = $id:fun($args:as'); |]
compC_CMD (InModule mod prog) = inModule mod prog

instance (CompExp exp, CompTypeClass ct)          => Interp RefCMD     CGen (Param2 exp ct) where interp = compRefCMD
instance (CompExp exp, CompTypeClass ct)          => Interp ArrCMD     CGen (Param2 exp ct) where interp = compArrCMD
instance (CompExp exp, CompTypeClass ct)          => Interp ControlCMD CGen (Param2 exp ct) where interp = compControlCMD
instance                                             Interp PtrCMD     CGen (Param2 exp ct) where interp = compPtrCMD
instance (CompExp exp, CompTypeClass ct, ct Bool) => Interp FileCMD    CGen (Param2 exp ct) where interp = compFileCMD
instance (CompExp exp, CompTypeClass ct)          => Interp C_CMD      CGen (Param2 exp ct) where interp = compC_CMD

