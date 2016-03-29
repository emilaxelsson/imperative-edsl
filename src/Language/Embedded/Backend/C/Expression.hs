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

#if MIN_VERSION_syntactic(3,0,0)
import Data.TypeRep hiding (Typeable, gcast)
import Data.TypeRep.TH
import Data.TypeRep.Types.Basic
import Data.TypeRep.Types.Tuple
import Data.TypeRep.Types.IntWord
#endif

import Language.C.Monad
import Language.C.Quote.C
import Language.C.Syntax (Exp,Type)
import qualified Language.C.Syntax as C

import Language.Embedded.Expression



-- | General interface for compiling expressions
class FreeExp exp => CompExp exp
  -- The super class is motivated by the fact that compilation of functions
  -- `exp a -> exp b` can be done by constructing an argument using `varExp`.
  where
    -- | Compilation of expressions
    compExp :: MonadC m => exp a -> m Exp

instance ToExp Int8   where toExp = toExp . toInteger
instance ToExp Int16  where toExp = toExp . toInteger
instance ToExp Int32  where toExp = toExp . toInteger
instance ToExp Int64  where toExp = toExp . toInteger
instance ToExp Word8  where toExp = toExp . toInteger
instance ToExp Word16 where toExp = toExp . toInteger
instance ToExp Word32 where toExp = toExp . toInteger
instance ToExp Word64 where toExp = toExp . toInteger
  -- See <https://github.com/mainland/language-c-quote/pull/63>

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

#if MIN_VERSION_syntactic(3,0,0)
instance ShowClass CType where showClass _ = "CType"

pCType :: Proxy CType
pCType = Proxy

deriveWitness ''CType ''BoolType
deriveWitness ''CType ''FloatType
deriveWitness ''CType ''DoubleType
deriveWitness ''CType ''IntWordType

derivePWitness ''CType ''BoolType
derivePWitness ''CType ''FloatType
derivePWitness ''CType ''DoubleType
derivePWitness ''CType ''IntWordType

instance PWitness CType CharType t
instance PWitness CType ListType t
instance PWitness CType TupleType t
instance PWitness CType FunType t
#endif

-- | Remove one layer of a nested proxy
proxyArg :: proxy1 (proxy2 a) -> Proxy a
proxyArg _ = Proxy

-- | Create and declare a fresh variable
freshVar :: forall m a . (MonadC m, CType a) => m (Val a)
freshVar = do
    v <- gensym "v"
    touchVar v
    t <- cType (Proxy :: Proxy a)
    case t of
      C.Type _ C.Ptr{} _ -> addLocal [cdecl| $ty:t $id:v = NULL; |]
      _                  -> addLocal [cdecl| $ty:t $id:v; |]
    return (ValComp v)

