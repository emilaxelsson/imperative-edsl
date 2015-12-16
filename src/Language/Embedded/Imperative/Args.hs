{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds   #-}

-- | Various types of function arguments

module Language.Embedded.Imperative.Args where

import Data.Proxy
import Language.C.Quote.C
import Language.C.Syntax
import Language.Embedded.Expression
import Language.Embedded.Imperative.CMD
import Language.Embedded.Backend.C

-- | Value argument
data ValArg exp where
  ValArg :: VarPred exp a => exp a -> ValArg exp

instance Arg ValArg where
  mkArg   (ValArg a) = compExp a
  mkParam (ValArg a) = do
    t <- compType a
    return [cparam| $ty:t |]

-- | Reference argument
data RefArg exp where
  RefArg :: VarPred exp a => Ref a -> RefArg exp

instance Arg RefArg where
  mkArg   (RefArg r) = return [cexp| &$id:r |]
  mkParam (RefArg (r :: Ref a) :: RefArg exp) = do
    t <- compTypeP (Proxy :: Proxy (exp a))
    return [cparam| $ty:t* |]

-- | Array argument
data ArrArg exp where
  ArrArg :: VarPred exp a => Arr n a -> ArrArg exp

instance Arg ArrArg where
  mkArg   (ArrArg a) = return [cexp| $id:a |]
  mkParam (ArrArg (a :: Arr n a) :: ArrArg exp) = do
    t <- compTypeP (Proxy :: Proxy (exp a))
    return [cparam| $ty:t* |]

-- | Abstract object argument
data ObjArg exp where
  ObjArg :: Object -> ObjArg exp

instance Arg ObjArg where
  mkArg   (ObjArg o) = return [cexp| $id:o |]
  mkParam (ObjArg (Object True t _))  = let t' = namedType t in return [cparam| $ty:t'* |]
  mkParam (ObjArg (Object False t _)) = let t' = namedType t in return [cparam| $ty:t' |]

-- | Constant string argument
data StrArg exp where
  StrArg :: String -> StrArg exp

instance Arg StrArg where
  mkArg   (StrArg s) = return [cexp| $string:s |]
  mkParam (StrArg s) = return [cparam| const char* |]

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
       _ -> error "Cannot deal with antiquotes"
