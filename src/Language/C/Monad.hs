-- Copyright (c) 2016 and after, see package copyright

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

{-# LANGUAGE CPP #-}
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

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Exception

import Language.C.Quote.C
import qualified Language.C.Syntax as C
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid
import Text.PrettyPrint.Mainland
import Data.Loc
import Data.List (partition,nub)

-- | Code generation flags
data Flags = Flags

-- | Code generator state.
data CEnv = CEnv
    { _flags       :: Flags

    , _unique      :: !Integer

    , _modules     :: Map.Map String [C.Definition]
    , _includes    :: Set.Set String
    , _typedefs    :: [C.Definition]
    , _prototypes  :: [C.Definition]
    , _globals     :: [C.Definition]

    , _aliases     :: Map.Map Integer String
    , _params      :: [C.Param]
    , _args        :: [C.Exp]
    , _locals      :: [C.InitGroup]
    , _items       :: [C.BlockItem]
    , _finalItems  :: [C.BlockItem]

    , _usedVars    :: Set.Set C.Id
    , _funUsedVars :: Map.Map String (Set.Set C.Id)
    }

makeLenses ''CEnv

-- | Default code generator state
defaultCEnv :: Flags -> CEnv
defaultCEnv fl = CEnv
    { _flags       = fl
    , _unique      = 0
    , _modules     = mempty
    , _includes    = mempty
    , _typedefs    = mempty
    , _prototypes  = mempty
    , _globals     = mempty
    , _aliases     = mempty
    , _params      = mempty
    , _args        = mempty
    , _locals      = mempty
    , _items       = mempty
    , _finalItems  = mempty
    , _usedVars    = mempty
    , _funUsedVars = mempty
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
        toInclude inc = [cedecl|$esc:include|]
          where include = "#include " ++ inc
    tds    = nub $ reverse $ _typedefs env
    protos = nub $ reverse $ _prototypes env
    globs  = nub $ reverse $ _globals env

-- | Generate C documents for each module
prettyCGenT :: Monad m => CGenT m a -> m [(String, Doc)]
prettyCGenT ma = do
    (_,cenv) <- runCGenT ma (defaultCEnv Flags)
    return $ map (("", ppr) <*>)
           $ ("main", cenvToCUnit cenv) : Map.toList (_modules cenv)

prettyCGen :: CGen a -> [(String, Doc)]
prettyCGen = runIdentity . prettyCGenT

-- | Retrieve a fresh identifier
freshId :: MonadC m => m Integer
freshId = unique <<%= succ

-- | Generate a fresh symbol by appending a fresh id to a base name
gensym :: MonadC m => String -> m String
gensym s = do
    u <- freshId
    return $ s ++ show u

-- | Mark an identifier as used in this context.
touchVar :: (MonadC m, ToIdent v) => v -> m ()
touchVar v = usedVars %= Set.insert (toIdent v (SrcLoc NoLoc))

-- | Set the 'Set' of identifers used in the body of the given function.
setUsedVars :: MonadC m => String -> Set.Set C.Id -> m ()
setUsedVars fun uvs = funUsedVars %= Map.insert fun uvs

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

-- | Let a variable be known by another name
withAlias :: MonadC m => Integer -> String -> m a -> m a
withAlias i n act = do
  oldAliases <- aliases <<%= Map.insert i n
  a <- act
  aliases .= oldAliases
  return a

-- | Add a function parameter when building a function definition
addParam :: MonadC m => C.Param -> m ()
addParam param = params %= (param:)

addParams :: MonadC m => [C.Param] -> m ()
addParams ps = params %= (reverse ps++)

-- | Add a function argument when building a function call
addArg :: MonadC m => C.Exp -> m ()
addArg arg = args %= (arg:)

-- | Add a local declaration (including initializations)
addLocal :: MonadC m => C.InitGroup -> m ()
addLocal def = do
  locals %= (def:)
  case def of
    C.InitGroup _ _ is _ -> forM_ is $ \(C.Init id _ _ _ _ _) -> touchVar id
    _                    -> return ()

-- | Add an item (a declaration or a statement) to the current block
--   This functionality is necessary to declare C99 variable-length arrays
--   in the middle of a block, as other local delcarations are lifted to the
--   beginning of the block, and that makes the evaluation of the length
--   expression impossible.
addItem :: MonadC m => C.BlockItem -> m ()
addItem item = items %= (item:)

-- | Add multiple local declarations
addLocals :: MonadC m => [C.InitGroup] -> m ()
addLocals defs = mapM_ addLocal defs -- locals %= (reverse defs++)

-- | Add a statement to the current block
addStm :: MonadC m => C.Stm -> m ()
addStm stm = items %= ((C.BlockStm stm):)

-- | Add a sequence of statements to the current block
addStms :: MonadC m => [C.Stm] -> m ()
addStms ss = items %= (reverse (map C.BlockStm ss)++)

-- | Add a statement to the end of the current block
addFinalStm :: MonadC m => C.Stm -> m ()
addFinalStm stm = finalItems %= ((C.BlockStm stm):)

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
    oldLocals     <- locals     <<.= mempty
    oldItems      <- items      <<.= mempty
    oldFinalItems <- finalItems <<.= mempty
    x <- ma
    ls  <- reverse <$> (locals     <<.= oldLocals)
    ss  <- reverse <$> (items      <<.= oldItems)
    fss <- reverse <$> (finalItems <<.= oldFinalItems)
    return (x, map C.BlockDecl ls  ++
               ss  ++
               fss
           )

-- | Run an action as a block and capture the items.
-- Does not place the items in an actual C block.
inNewBlock_ :: MonadC m => m a -> m [C.BlockItem]
inNewBlock_ ma = snd <$> inNewBlock ma

-- | Run an action as a function declaration.
-- Does not create a new function.
inNewFunction :: MonadC m => m a -> m (a,Set.Set C.Id,[C.Param],[C.BlockItem])
inNewFunction comp = do
    oldParams <- params <<.= mempty
    oldUsedVars <- usedVars <<.= mempty
    (a,items)  <- inNewBlock comp
    ps <- params <<.= oldParams
    uvs <- usedVars <<.= oldUsedVars
    return (a, uvs, reverse ps, items)

-- | Declare a function
inFunction :: MonadC m => String -> m a -> m a
inFunction = inFunctionTy [cty|void|]

-- | Declare a function with the given return type.
inFunctionTy :: MonadC m => C.Type -> String -> m a -> m a
inFunctionTy ty fun ma = do
    (a,uvs,ps,items) <- inNewFunction ma
    setUsedVars fun uvs
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
    (_,uvs,params,items) <- inNewFunction $ prog >> addStm [cstm| return 0; |]
    setUsedVars "main" uvs
    addGlobal [cedecl| int main($params:params){ $items:items }|]

-- | Lift the declarations of all variables that are shared between functions
--   to the top level. This relies on variable IDs being unique across
--   programs, not just across the functions in which they are declared.
--
--   Only affects locally declared vars, not function arguments.
liftSharedLocals :: MonadC m => m a -> m ()
liftSharedLocals prog = do
    prog
    uvs <- Set.unions . Map.elems . onlyShared . _funUsedVars <$> get
    -- This could be more efficient by just filtering each function for the
    -- vars we *know* are in there, provided that we had a Map from function
    -- names to bodies.
    oldglobs <- _globals <$> get
    let (globs, shared) = unzip $ map (extractDecls (`Set.member` uvs)) oldglobs
        sharedList = Set.toList $ Set.unions shared
        sharedDecls = map (\ig -> C.DecDef ig (SrcLoc NoLoc)) sharedList
    -- Reverse is a trick that ensures the correct order of declarations for arrays
    -- and their wrapper pointers. It depends on the naming schema of identifiers:
    -- arrays are prefixed with underscores, while their wrappers are not.
    void $ globals <<.= (globs ++ reverse sharedDecls)
  where
    -- Only keep vars shared between functions by intersecting with the union
    -- of all other funs' uvs. TODO: optimize.
    onlyShared :: Map.Map String (Set.Set C.Id) -> Map.Map String (Set.Set C.Id)
    onlyShared alluvs =
        Map.mapWithKey funUVSIntersects alluvs
      where
        funUVSIntersects fun uvs =
          Set.intersection uvs $ Set.unions $ Map.elems $ Map.delete fun alluvs

-- | Remove all declarations matching a predicate from the given function
--   and return them in a separate list.
extractDecls :: (C.Id -> Bool)
             -> C.Definition
             -> (C.Definition, Set.Set C.InitGroup)
extractDecls pred (C.FuncDef (C.Func ds id decl params bis loc') loc) =
  case foldr perBI ([], Set.empty) bis of
    (bis', igs) -> (C.FuncDef (C.Func ds id decl params bis' loc') loc, igs)
  where
    perBI decl@(C.BlockDecl ig@(C.InitGroup ds attrs is loc)) (bis, igs) =
      case partition (\(C.Init id _ _ _ _ _) -> pred id) is of
        ([], unmach) ->
          (decl : bis, igs)
        (match, []) ->
          (bis, Set.insert ig igs)
        (match, unmatch) ->
          (C.BlockDecl (C.InitGroup ds attrs unmatch loc) : bis,
           Set.insert (C.InitGroup ds attrs match loc) igs)
    perBI bi (bis, igs) =
      (bi:bis, igs)
extractDecls _ decl =
  (decl, Set.empty)
