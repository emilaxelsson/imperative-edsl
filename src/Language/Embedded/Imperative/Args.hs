{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds   #-}

-- | Various types of function arguments

module Language.Embedded.Imperative.Args where

import Control.Monad
import Data.Proxy
import Language.C.Quote.C
import Language.C.Syntax hiding (Deref)
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Backend.C

-- | Value argument
data ValArg exp where
  ValArg :: VarPred exp a => exp a -> ValArg exp

instance Arg ValArg where
  mkArg   (ValArg a) = compExp a
  mkParam (ValArg (a :: exp a)) = do
    t <- compType (Proxy :: Proxy exp) (Proxy :: Proxy a)
    return [cparam| $ty:t |]

  mapArg predCast f (ValArg (a :: exp a)) =
    predCast (Proxy :: Proxy a) $ ValArg (f a)

  mapMArg predCast f (ValArg (a :: exp a)) =
    predCast (Proxy :: Proxy a) $ liftM ValArg (f a)

-- | Reference argument
data RefArg exp where
  RefArg :: VarPred exp a => Ref a -> RefArg exp

instance Arg RefArg where
  mkArg   (RefArg r) = return [cexp| &$id:r |]
  mkParam (RefArg (r :: Ref a) :: RefArg exp) = do
    t <- compType (Proxy :: Proxy exp) (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

  mapArg predCast _ (RefArg (r :: Ref a)) =
    predCast (Proxy :: Proxy a) $ RefArg r

  mapMArg predCast _ (RefArg (r :: Ref a)) =
    predCast (Proxy :: Proxy a) $ return $ RefArg r

-- | Mutable array argument
data ArrArg exp where
  ArrArg :: VarPred exp a => Arr i a -> ArrArg exp

instance Arg ArrArg where
  mkArg   (ArrArg a) = return [cexp| $id:a |]
  mkParam (ArrArg (_ :: Arr i a) :: ArrArg exp) = do
    t <- compType (Proxy :: Proxy exp) (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

  mapArg predCast _ (ArrArg (a :: Arr i a)) =
    predCast (Proxy :: Proxy a) $ ArrArg a

  mapMArg predCast _ (ArrArg (a :: Arr i a)) =
    predCast (Proxy :: Proxy a) $ return $ ArrArg a

-- | Immutable array argument
data IArrArg exp where
  IArrArg :: VarPred exp a => IArr i a -> IArrArg exp

instance Arg IArrArg where
  mkArg   (IArrArg a) = return [cexp| $id:a |]
  mkParam (IArrArg (_ :: IArr i a) :: IArrArg exp) = do
    t <- compType (Proxy :: Proxy exp) (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

  mapArg predCast _ (IArrArg (a :: IArr i a)) =
    predCast (Proxy :: Proxy a) $ IArrArg a

  mapMArg predCast _ (IArrArg (a :: IArr i a)) =
    predCast (Proxy :: Proxy a) $ return $ IArrArg a

-- | Pointer argument
data PtrArg exp where
  PtrArg :: VarPred exp a => Ptr a -> PtrArg exp

instance Arg PtrArg where
  mkArg   (PtrArg p) = return [cexp| $id:p |]
  mkParam (PtrArg (_ :: Ptr a) :: PtrArg exp) = do
    t <- compType (Proxy :: Proxy exp) (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

  mapArg predCast _ (PtrArg (p :: Ptr a)) =
    predCast (Proxy :: Proxy a) $ PtrArg p

  mapMArg predCast _ (PtrArg (p :: Ptr a)) =
    predCast (Proxy :: Proxy a) $ return $ PtrArg p

-- | Abstract object argument
data ObjArg exp where
  ObjArg :: Object -> ObjArg exp

instance Arg ObjArg where
  mkArg   (ObjArg o) = return [cexp| $id:o |]
  mkParam (ObjArg (Object True t _))  = let t' = namedType t in return [cparam| $ty:t'* |]
  mkParam (ObjArg (Object False t _)) = let t' = namedType t in return [cparam| $ty:t' |]
  mapArg  _ _ (ObjArg o) = ObjArg o
  mapMArg _ _ (ObjArg o) = return $ ObjArg o


-- | Constant string argument
data StrArg exp where
  StrArg :: String -> StrArg exp

instance Arg StrArg where
  mkArg   (StrArg s) = return [cexp| $string:s |]
  mkParam (StrArg s) = return [cparam| const char* |]
  mapArg  _ _ (StrArg s) = StrArg s
  mapMArg _ _ (StrArg s) = return $ StrArg s

-- | Modifier that takes the address of another argument
newtype Addr arg exp = Addr (arg exp)

instance Arg arg => Arg (Addr arg) where
  mkArg (Addr arg) = do
    e <- mkArg arg
    return [cexp| &$e |]
  mkParam (Addr arg) = do
    p <- mkParam arg
    case p of
       Param mid spec decl loc -> return $ Param mid spec (Ptr [] decl loc) loc
       _ -> error "mkParam for Addr: cannot deal with antiquotes"
  mapArg  predCast f (Addr arg) = Addr (mapArg predCast f arg)
  mapMArg predCast f (Addr arg) = liftM Addr (mapMArg predCast f arg)

-- | Modifier that dereferences another argument
newtype Deref arg exp = Deref (arg exp)

instance Arg arg => Arg (Deref arg) where
  mkArg (Deref arg) = do
    e <- mkArg arg
    return [cexp| *$e |]
  mkParam (Deref arg) = do
    p <- mkParam arg
    case p of
       Param mid spec (Ptr [] decl _) loc -> return $ Param mid spec decl loc
       Param _ _ _ _ -> error "mkParam for Deref: cannot dereference non-pointer parameter"
       _ -> error "mkParam for Deref: cannot deal with antiquotes"
  mapArg  predCast f (Deref arg) = Deref (mapArg predCast f arg)
  mapMArg predCast f (Deref arg) = liftM Deref (mapMArg predCast f arg)

