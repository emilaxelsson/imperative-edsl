{-# LANGUAGE QuasiQuotes #-}

-- | Various types of function arguments

module Language.Embedded.Imperative.Args where

import Data.Proxy

import Language.C.Quote.C

import Language.C.Monad
import Language.Embedded.Imperative.CMD
import Language.Embedded.Backend.C

-- | Reference argument
data RefArg pred where
  RefArg :: pred a => Ref a -> RefArg pred

instance Arg RefArg CType where
  mkArg   (RefArg r) = touchVar r >> return [cexp| &$id:r |]
  mkParam (RefArg (r :: Ref a)) = do
    t <- cType (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

-- | Mutable array argument
data ArrArg pred where
  ArrArg :: pred a => Arr i a -> ArrArg pred

instance Arg ArrArg CType where
  mkArg   (ArrArg a) = touchVar a >> return [cexp| $id:a |]
  mkParam (ArrArg (_ :: Arr i a)) = do
    t <- cType (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

-- | Immutable array argument
data IArrArg pred where
  IArrArg :: pred a => IArr i a -> IArrArg pred

instance Arg IArrArg CType where
  mkArg   (IArrArg a) = touchVar a >> return [cexp| $id:a |]
  mkParam (IArrArg (_ :: IArr i a)) = do
    t <- cType (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

-- | Pointer argument
data PtrArg pred where
  PtrArg :: pred a => Ptr a -> PtrArg pred

instance Arg PtrArg CType where
  mkArg   (PtrArg p) = touchVar p >> return [cexp| $id:p |]
  mkParam (PtrArg (_ :: Ptr a)) = do
    t <- cType (Proxy :: Proxy a)
    return [cparam| $ty:t* |]

-- | Abstract object argument
data ObjArg pred where
  ObjArg :: Object -> ObjArg pred

instance Arg ObjArg pred where
  mkArg   (ObjArg o) = touchVar o >> return [cexp| $id:o |]
  mkParam (ObjArg (Object pointed t _))
      | pointed   = return [cparam| $ty:t'* |]
      | otherwise = return [cparam| $ty:t'  |]
    where
      t' = namedType t

-- | Constant string argument
data StrArg pred where
  StrArg :: String -> StrArg pred

instance Arg StrArg pred where
  mkArg   (StrArg s) = return [cexp| $string:s |]
  mkParam (StrArg s) = return [cparam| const char* |]

