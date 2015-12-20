{-# LANGUAGE TypeOperators #-}

module Imperative where



import Control.Applicative ((<$>))
import Data.Int
import Data.Word

import Language.Embedded.Expression (evalExp)
import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import Language.Embedded.CExp



refProg :: Program (RefCMD CExp) (CExp Int32)
refProg = do
    r1 <- initRef 4
    r2 <- initRef 5
    a  <- unsafeFreezeRef r1
    b  <- getRef r2
    let c = (a #== 10 ? a+b $ b+a) + 3
    setRef r2 c
    return c

evalRef :: IO Int32
evalRef = fmap evalExp $ runIO refProg

compRef = icompile refProg

type CMD1
    =   RefCMD CExp
    :+: ArrCMD CExp
    :+: ControlCMD CExp

arrProg :: Program CMD1 (CExp Int32)
arrProg = do
    ref <- initRef 4
    arr <- newArr (10 :: CExp Word8)
    setArr 3 45 arr
    a   <- unsafeFreezeRef ref
    b   <- getArr 3 arr
    let c = a+b
    iff (a #== 4)
      (setRef ref c)
      (setRef ref b)
    return c

evalArr :: IO Int32
evalArr = fmap evalExp $ runIO arrProg

compArr = icompile arrProg



type CMD2
    =   RefCMD CExp
    :+: ControlCMD CExp
    :+: FileCMD CExp

sumInput :: Program CMD2 ()
sumInput = do
    done <- initRef false
    sum  <- initRef (0 :: CExp Word32)
    while (not_ <$> getRef done) $ do
        printf "Enter a number (0 means done): "
        n <- fget stdin
        iff (n #== 0)
          (setRef done true)
          (modifyRef sum (+n))
    printf "The sum of your numbers is %d.\n" =<< getRef sum

run_sumInput = runCompiled [] sumInput []

