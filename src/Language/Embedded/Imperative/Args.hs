{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds   #-}

-- | Various types of function arguments

module Language.Embedded.Imperative.Args where

import Data.Proxy
import Data.Typeable
import Language.C.Quote.C
import Language.C.Syntax
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Backend.C

-- | Value argument
data ValArg pred exp where
  ValArg :: (pred a, VarPred exp a) => exp a -> ValArg pred exp

instance CompExp exp => Arg ValArg exp where
  mkArg   (ValArg a) = compExp a
  mkParam (ValArg a) = do
    t <- compType a
    return [cparam| $ty:t |]
  weakenArg (ValArg a) = ValArg a

-- | Reference argument
data RefArg pred exp where
  RefArg :: (pred a, Typeable a, VarPred exp a, CompExp exp) =>
    Ref a -> RefArg pred exp

instance CompExp exp => Arg RefArg exp where
  mkArg   (RefArg r) = return [cexp| &$id:r |]
  mkParam (RefArg (r :: Ref a)) = do
    t <- compTypeP (Proxy :: Proxy (exp a))
    return [cparam| $ty:t* |]
  weakenArg (RefArg r) = RefArg r

-- | Array argument
data ArrArg pred exp where
  ArrArg :: (pred a, Typeable a, VarPred exp a) =>
    Arr n a -> ArrArg pred exp

instance CompExp exp => Arg ArrArg exp where
  mkArg   (ArrArg a) = return [cexp| $id:a |]
  mkParam (ArrArg (a :: Arr n a)) = do
    t <- compTypeP (Proxy :: Proxy (exp a))
    return [cparam| $ty:t* |]
  weakenArg (ArrArg a) = ArrArg a

-- | Abstract object argument
data ObjArg pred exp where
  ObjArg :: Object -> ObjArg pred exp

instance Arg ObjArg exp where
  mkArg     (ObjArg (Object _ _ o)) = return [cexp| $id:o |]
  mkParam   (ObjArg (Object True t _))  = let t' = namedType t in return [cparam| $ty:t'* |]
  mkParam   (ObjArg (Object False t _)) = let t' = namedType t in return [cparam| $ty:t' |]
  weakenArg (ObjArg o) = ObjArg o

-- | Constant string argument
data StrArg pred exp where
  StrArg :: String -> StrArg pred exp

instance Arg StrArg exp where
  mkArg     (StrArg s) = return [cexp| $string:s |]
  mkParam   (StrArg s) = return [cparam| const char* |]
  weakenArg (StrArg s) = StrArg s

-- | Modifier that takes the address of another argument
data Addr arg pred exp where
  Addr :: Arg arg exp => arg pred exp -> Addr arg pred exp

instance Arg arg exp => Arg (Addr arg) exp where
  mkArg (Addr arg) = do
    e <- mkArg arg
    return [cexp| &$e |]
  mkParam (Addr arg) = do
    p <- mkParam arg
    case p of
       Param mid spec decl loc -> return $ Param mid spec (Ptr [] decl loc) loc
       _ -> error "Cannot deal with antiquotes"
  weakenArg (Addr arg) = Addr (weakenArg arg)
