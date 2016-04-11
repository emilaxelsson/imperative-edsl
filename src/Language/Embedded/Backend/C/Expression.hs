{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Embedded.Backend.C.Expression where



import Data.Int
import Data.Word
import Data.Proxy
import Data.Typeable
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif

import Language.C.Monad
import Language.C.Quote.C
import Language.C.Syntax (Exp,Type)
import qualified Language.C.Syntax as C

import Control.Monad.Operational.Higher

import Language.Embedded.Expression



-- | General interface for compiling expressions
class FreeExp exp => CompExp exp
  -- The super class is motivated by the fact that compilation of functions
  -- `exp a -> exp b` can be done by constructing an argument using `varExp`.
  where
    -- | Compilation of expressions
    compExp :: MonadC m => exp a -> m Exp

-- | Types supported by C
class (Show a, Eq a, Typeable a) => CType a
  where
    cType :: MonadC m => proxy a -> m Type

    cLit         :: MonadC m => a -> m Exp
    default cLit :: (ToExp a, MonadC m) => a -> m Exp
    cLit = return . flip toExp mempty

instance CType Bool
  where
    cType _ = do
        addSystemInclude "stdbool.h"
        return [cty| typename bool |]
    cLit b = do
        addSystemInclude "stdbool.h"
        return $ if b then [cexp| true |] else [cexp| false |]

instance CType Int8   where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int8_t   |]
instance CType Int16  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int16_t  |]
instance CType Int32  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int32_t  |]
instance CType Int64  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename int64_t  |]
instance CType Word8  where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint8_t  |]
instance CType Word16 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint16_t |]
instance CType Word32 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint32_t |]
instance CType Word64 where cType _ = addSystemInclude "stdint.h"  >> return [cty| typename uint64_t |]

instance CType Float  where cType _ = return [cty| float |]
instance CType Double where cType _ = return [cty| double |]

-- | Remove one layer of a nested proxy
proxyArg :: proxy1 (proxy2 a) -> Proxy a
proxyArg _ = Proxy

-- | Classes that support reification to C types
class CompTypeClass ct
  where
    compType :: (ct a, MonadC m) => proxy1 ct -> proxy2 a -> m Type
    compLit  :: (ct a, MonadC m) => proxy ct -> a -> m Exp

instance CompTypeClass CType
  where
    compType _ = cType
    compLit _  = cLit

-- | Get the type predicate from an instruction type
proxyPred :: cmd (Param3 p e pred) a -> Proxy pred
proxyPred _ = Proxy

-- | Create and declare a fresh variable
freshVar :: forall m ct proxy a . (MonadC m, CompTypeClass ct, ct a) =>
    proxy ct -> m (Val a)
freshVar ct = do
    v <- gensym "v"
    touchVar v
    t <- compType ct (Proxy :: Proxy a)
    case t of
      C.Type _ C.Ptr{} _ -> addLocal [cdecl| $ty:t $id:v = NULL; |]
      _                  -> addLocal [cdecl| $ty:t $id:v; |]
    return (ValComp v)

