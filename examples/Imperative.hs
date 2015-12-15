{-# LANGUAGE TypeOperators #-}

module Imperative where



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
    let c = a+b
    setRef r2 c
    return c

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

evalRef :: IO Int32
evalRef = fmap evalExp $ runIO refProg

compRef = icompile refProg

evalArr :: IO Int32
evalArr = fmap evalExp $ runIO arrProg

compArr = icompile arrProg



type CMD2
    =   RefCMD CExp
    :+: ControlCMD CExp
    :+: FileCMD CExp

summer :: Program CMD2 ()
summer = do
    inp <- fopen "input" ReadMode
    let cont = fmap not_ $ feof inp
    sum <- initRef (0 :: CExp Float)
    while cont $ do
        f <- fget inp
        s <- getRef sum
        setRef sum (s+f+(3+4+5+6))
    s <- getRef sum
    printf "The sum is: %f\n" s

runSummer :: IO ()
runSummer = do
    writeFile "input" $ unwords $ map show ([-5..4] :: [Float])
    runIO summer

compSummer = icompile summer

