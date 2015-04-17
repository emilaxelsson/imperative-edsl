-- Copyright (c) 2009-2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

-- Copyright (c) 2011-2012, Geoffrey Mainland
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:
-- 1. Redistributions of source code must retain the above copyright notice, this
--    list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
-- ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- Copyright (c) 2015, Anders Persson
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Anders Persson nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A monad for C code generation
module Language.C.Monad
  where

import Control.Lens
import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Exception

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import Text.PrettyPrint.Mainland

-- | Code generation flags
data Flags = Flags

-- | Code generator state.
data CEnv = CEnv
    { _flags      :: Flags

    , _unique     :: !Integer

    , _modules    :: Map.Map String [C.Definition]
    , _includes   :: Set.Set String
    , _typedefs   :: [C.Definition]
    , _prototypes :: [C.Definition]
    , _globals    :: [C.Definition]

    , _params     :: [C.Param]
    , _args       :: [C.Exp]
    , _locals     :: [C.InitGroup]
    , _stms       :: [C.Stm]
    , _finalStms  :: [C.Stm]
    }

makeLenses ''CEnv

-- | Default code generator state
defaultCEnv :: Flags -> CEnv
defaultCEnv fl = CEnv
    { _flags      = fl
    , _unique     = 0
    , _modules    = mempty
    , _includes   = mempty
    , _typedefs   = mempty
    , _prototypes = mempty
    , _globals    = mempty
    , _params     = mempty
    , _args       = mempty
    , _locals     = mempty
    , _stms       = mempty
    , _finalStms  = mempty
    }

-- | Code generation type constraints
type MonadC m = (Functor m, Applicative m, Monad m, MonadState CEnv m, MonadException m, MonadFix m)

-- | The C code generation monad transformer
newtype CGenT t a = CGenT { unCGenT :: StateT CEnv (ExceptionT t) a }
  deriving (Functor, Applicative, Monad, MonadException, MonadState CEnv, MonadIO, MonadFix)

type CGen = CGenT Identity

-- | Run the C code generation monad
runCGenT :: Monad m => CGenT m a -> CEnv -> m (a, CEnv)
runCGenT m s = do
    Right ac <- runExceptionT (runStateT (unCGenT m) s)
    return ac

-- | Run the C code generation monad
runCGen :: CGen a -> CEnv -> (a, CEnv)
runCGen m = runIdentity . runCGenT m

-- | Extract a compilation unit from the 'CEnv' state
cenvToCUnit :: CEnv -> [C.Definition]
cenvToCUnit env =
    [cunit|$edecls:incs
           $edecls:tds
           $edecls:protos
           $edecls:globs|]
  where
    incs = map toInclude (Set.toList (_includes env))
      where
        toInclude :: String -> C.Definition
        toInclude inc = [cedecl|$esc:("#include " ++ inc)|]
    tds    = reverse $ _typedefs env
    protos = reverse $ _prototypes env
    globs  = reverse $ _globals env

-- | Generate a C document
prettyCGenT :: Monad m => CGenT m a -> m Doc
prettyCGenT ma = do
    (_,cenv) <- runCGenT ma (defaultCEnv Flags)
    return $ ppr $ cenvToCUnit cenv

prettyCGen :: CGen a -> Doc
prettyCGen = runIdentity . prettyCGenT

-- | Retrieve a fresh identifier
freshId :: MonadC m => m Integer
freshId = unique <<%= succ

-- | Generate a fresh symbol by appending a fresh id to a base name
gensym :: MonadC m => String -> m String
gensym s = do
    u <- freshId
    return $ s ++ show u

-- | Add an include pre-processor directive. Specify '<>' or '""' around
-- the file name.
addInclude :: MonadC m => String -> m ()
addInclude inc = includes %= Set.insert inc

-- | Add a local include directive. The argument will be surrounded by '""'
addLocalInclude :: MonadC m => String -> m ()
addLocalInclude inc = addInclude ("\"" ++ inc ++ "\"")

-- | Add a system include directive. The argument will be surrounded by '<>'
addSystemInclude :: MonadC m => String -> m ()
addSystemInclude inc = addInclude ("<" ++ inc ++ ">")

-- | Add a type definition
addTypedef :: MonadC m => C.Definition -> m ()
addTypedef def = typedefs %= (def:)

-- | Add a function prototype
addPrototype :: MonadC m => C.Definition -> m ()
addPrototype def = prototypes %= (def:)

-- | Add a global definition
addGlobal :: MonadC m => C.Definition -> m ()
addGlobal def = globals %= (def:)

-- | Add multiple global definitions
addGlobals :: MonadC m => [C.Definition] -> m ()
addGlobals defs = globals %= (defs++)

-- | Add a function parameter when building a function definition
addParam :: MonadC m => C.Param -> m ()
addParam param = params %= (param:)

-- | Add a function argument when building a function call
addArg :: MonadC m => C.Exp -> m ()
addArg arg = args %= (arg:)

-- | Add a local declaration (including initializations)
addLocal :: MonadC m => C.InitGroup -> m ()
addLocal def = locals %= (def:)

-- | Add multiple local declarations
addLocals :: MonadC m => [C.InitGroup] -> m ()
addLocals defs = locals %= (reverse defs++)

-- | Add a statement to the current block
addStm :: MonadC m => C.Stm -> m ()
addStm stm = stms %= (stm:)

-- | Add a sequence of statements to the current block
addStms :: MonadC m => [C.Stm] -> m ()
addStms ss = stms %= (reverse ss++)

-- | Add a statement to the end of the current block
addFinalStm :: MonadC m => C.Stm -> m ()
addFinalStm stm = finalStms %= (stm:)

-- | Run an action in a new block
inBlock :: MonadC m => m a -> m a
inBlock ma = do
    (a, items) <- inNewBlock ma
    addStm [cstm|{ $items:items }|]
    return a

-- | Run an action as a block and capture the items.
-- Does not place the items in an actual C block.
inNewBlock :: MonadC m => m a -> m (a, [C.BlockItem])
inNewBlock ma = do
    oldLocals    <- locals    <<.= mempty
    oldStms      <- stms      <<.= mempty
    oldFinalStms <- finalStms <<.= mempty
    x <- ma
    ls  <- reverse <$> (locals    <<.= oldLocals)
    ss  <- reverse <$> (stms      <<.= oldStms)
    fss <- reverse <$> (finalStms <<.= oldFinalStms)
    return (x, map C.BlockDecl ls  ++
               map C.BlockStm  ss  ++
               map C.BlockStm  fss
           )

-- | Run an action as a block and capture the items.
-- Does not place the items in an actual C block.
inNewBlock_ :: MonadC m => m a -> m [C.BlockItem]
inNewBlock_ ma = snd <$> inNewBlock ma

-- | Run an action as a function declaration.
-- Does not create a new function.
inNewFunction :: MonadC m => m a -> m (a,[C.Param],[C.BlockItem])
inNewFunction comp = do
    oldParams <- params <<.= mempty
    (a,items)  <- inNewBlock comp
    ps <- params <<.= oldParams
    return (a, reverse ps, items)

-- | Declare a function
inFunction :: MonadC m => String -> m a -> m a
inFunction = inFunctionTy [cty|void|]

-- | Declare a function with the given return type.
inFunctionTy :: MonadC m => C.Type -> String -> m a -> m a
inFunctionTy ty fun ma = do
    (a,ps,items) <- inNewFunction ma
    addPrototype [cedecl| $ty:ty $id:fun($params:ps);|]
    addGlobal [cedecl| $ty:ty $id:fun($params:ps){ $items:items }|]
    return a

-- | Collect all global definitions in the current state
collectDefinitions :: MonadC m => m a -> m (a, [C.Definition])
collectDefinitions ma = do
    oldIncludes   <- includes   <<.= mempty
    oldTypedefs   <- typedefs   <<.= mempty
    oldPrototypes <- prototypes <<.= mempty
    oldGlobals    <- globals    <<.= mempty
    a  <- ma
    s' <- get
    modify $ \s -> s { _includes   = oldIncludes    -- <> _includes s'
                     , _typedefs   = oldTypedefs    -- <> _typedefs s'
                     , _prototypes = oldPrototypes  -- <> _prototypes s'
                     , _globals    = oldGlobals     -- <> _globals s'
                     }
    return (a, cenvToCUnit s')

-- | Collect all function arguments in the current state
collectArgs :: MonadC m => m [C.Exp]
collectArgs = args <<.= mempty

-- | Declare a C translation unit
inModule :: MonadC m => String -> m a -> m a
inModule name prg = do
    oldUnique <- unique <<.= 0
    (a, defs) <- collectDefinitions prg
    unique .= oldUnique
    modules %= Map.insertWith (<>) name defs
    return a

-- | Wrap a program in a main function
wrapMain :: MonadC m => m a -> m ()
wrapMain prog = do
    (_,params,items) <- inNewFunction $ prog >> addStm [cstm| return 0; |]
    addGlobal [cedecl| int main($params:params){ $items:items }|]

