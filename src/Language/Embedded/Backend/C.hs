{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generate C code from @Language.Embedded.Imperative@ programs
module Language.Embedded.Backend.C where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Proxy

import Data.Loc (noLoc)
import Language.C.Quote.C
import qualified Language.C.Syntax as C

import Control.Monad.Operational.Compositional
import Data.TypePredicates
import Language.C.Monad
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Imperative.Frontend.General

import Text.PrettyPrint.Mainland (pretty)

-- | Identifiers from references
instance ToIdent (Ref a)
  where
    toIdent (RefComp r) = C.Id $ 'v':show r

-- | Compile `RefCMD`
compRefCMD :: forall exp prog a. CompExp exp
           => RefCMD exp prog a -> CGen a
compRefCMD cmd@NewRef = do
    t <- compTypePP2 (Proxy :: Proxy exp) cmd
    r <- RefComp <$> freshId
    case t of
      C.Type _ C.Ptr{} _ -> addLocal [cdecl| $ty:t $id:r = NULL; |]
      _                  -> addLocal [cdecl| $ty:t $id:r; |]
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
    touchVar ref
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
           => ArrCMD exp prog a -> CGen a
compArrCMD cmd@(NewArr size) = do
    sym <- gensym "a"
    v   <- compExp size
    t   <- compTypePP2 (Proxy :: Proxy exp) cmd
    addLocal [cdecl| $ty:t $id:sym[ $v ]; |]
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
compControlCMD (For lo hi body) = do
    loe   <- compExp lo
    hie   <- compExp hi
    (i,n) <- freshVar
    bodyc <- inNewBlock_ (body i)
    addStm [cstm| for ($id:n=$loe; $id:n<=$hie; $id:n++) {$items:bodyc} |]
compControlCMD Break = addStm [cstm| break; |]

compIOMode :: IOMode -> String
compIOMode ReadMode      = "r"
compIOMode WriteMode     = "w"
compIOMode AppendMode    = "a"
compIOMode ReadWriteMode = "r+"

-- | Compile `FileCMD`
compFileCMD :: CompExp exp => FileCMD exp CGen a -> CGen a
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
    let h'     = [cexp| $id:h |]
        form'  = show form
        form'' = [cexp| $id:form' |]
    as' <- fmap ([h',form'']++) $ sequence [compExp a | ValArg a <- as]
    addStm [cstm| fprintf($args:as'); |]
compFileCMD cmd@(FGet (HandleComp h)) = do
    (v,n) <- freshVar
    touchVar h
    let mkProxy = (\_ -> Proxy) :: FileCMD exp prog (exp a) -> Proxy a
        form    = formatSpecifier (mkProxy cmd)
    addStm [cstm| fscanf($id:h, $string:form, &$id:n); |]
    return v
compFileCMD (FEof (HandleComp h)) = do
    addInclude "<stdbool.h>"
    (v,n) <- freshVar
    touchVar h
    addStm [cstm| $id:n = feof($id:h); |]
    return v

namedType :: String -> C.Type
namedType t = C.Type
    (C.DeclSpec [] [] (C.Tnamed (C.Id t noLoc) [] noLoc) noLoc)
    (C.DeclRoot noLoc)
    noLoc

compObjectCMD :: ObjectCMD CGen a -> CGen a
compObjectCMD (NewObject t) = do
    sym <- gensym "obj"
    let t' = namedType t
    addLocal [cdecl| $ty:t' * $id:sym; |]
    return $ Object t sym

mkArg :: CompExp exp => FunArg Any exp -> CGen C.Exp
mkArg (ValArg a) = compExp a
mkArg (RefArg r) = return [cexp| &$id:r |]
mkArg (ArrArg a) = return [cexp| $id:a |]
mkArg (ObjArg     (Object _ o)) = return [cexp| $id:o |]
mkArg (ObjAddrArg (Object _ o)) = return [cexp| &$id:o |]
mkArg (StrArg s) = return [cexp| $string:s |]

mkArgParam :: forall exp . CompExp exp => FunArg (VarPred exp) exp -> CGen C.Param
mkArgParam (ValArg a) = do
    t <- compType a
    return [cparam| $ty:t |]
mkArgParam (RefArg (r :: Ref a)) = do
    t <- compTypeP (Proxy :: Proxy (exp a))
    return [cparam| $ty:t* |]
mkArgParam (ArrArg (a :: Arr n a)) = do
    t <- compTypeP (Proxy :: Proxy (exp a))
    return [cparam| $ty:t* |]
mkArgParam (ObjArg     (Object t _)) = let t' = namedType t in return [cparam| $ty:t'* |]
mkArgParam (ObjAddrArg (Object t _)) = let t' = namedType t in return [cparam| $ty:t'** |]
mkArgParam (StrArg s) = return [cparam| const char* |]

compCallCMD :: CompExp exp => CallCMD exp CGen a -> CGen a
compCallCMD (AddInclude inc)    = addInclude inc
compCallCMD (AddDefinition def) = addGlobal def
compCallCMD (AddExternFun fun res args) = do
    tres  <- compTypeP res
    targs <- mapM mkArgParam args
    addGlobal [cedecl| extern $ty:tres $id:fun($params:targs); |]
compCallCMD (AddExternProc proc args) = do
    targs <- mapM mkArgParam args
    addGlobal [cedecl| extern void $id:proc($params:targs); |]
compCallCMD (CallFun fun as) = do
    as'   <- mapM mkArg as
    (v,n) <- freshVar
    addStm [cstm| $id:n = $id:fun($args:as'); |]
    return v
compCallCMD (CallProc fun as) = do
    as' <- mapM mkArg as
    addStm [cstm| $id:fun($args:as'); |]

instance CompExp exp => Interp (RefCMD exp)     CGen where interp = compRefCMD
instance CompExp exp => Interp (ArrCMD exp)     CGen where interp = compArrCMD
instance CompExp exp => Interp (ControlCMD exp) CGen where interp = compControlCMD
instance CompExp exp => Interp (FileCMD exp)    CGen where interp = compFileCMD
instance                Interp ObjectCMD        CGen where interp = compObjectCMD
instance CompExp exp => Interp (CallCMD exp)    CGen where interp = compCallCMD

-- | Compile a program to C code represented as a string
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
compile :: (Interp instr CGen, HFunctor instr) => Program instr a -> String
compile = pretty 80 . prettyCGen . liftSharedLocals . wrapMain . interpret

-- | Compile a program to C code and print it on the screen
--
-- For programs that make use of the primitives in
-- "Language.Embedded.Concurrent", the resulting C code can be compiled as
-- follows:
--
-- > gcc -Iinclude csrc/chan.c -lpthread YOURPROGRAM.c
icompile :: (Interp instr CGen, HFunctor instr) => Program instr a -> IO ()
icompile = putStrLn . compile

