{-# LANGUAGE TypeOperators #-}

module Imperative where



import Data.Word

import Language.Embedded.Expression (evalExp)
import Language.Embedded.Imperative
import Language.Embedded.Expr



refProg :: Program (RefCMD Expr) (Expr Int)
refProg = do
    r1 <- initRef 4
    r2 <- initRef 5
    b  <- getRef r2
    let c = Add (unsafeFreezeRef r1) b
    setRef r2 c
    return c

type CMD1
    =   RefCMD Expr
    :+: ArrCMD Expr
    :+: ControlCMD Expr

arrProg :: Program CMD1 (Expr Int)
arrProg = do
    ref <- initRef 4
    arr <- newArr (10:: Expr Word8) 6
    b   <- getArr 3 arr
    let a = unsafeFreezeRef ref
        c = Add a b
    iff (Eq a 4)
      (setRef ref c)
      (setRef ref b)
    return c

evalRef :: IO Int
evalRef = fmap evalExp $ interpret refProg

compRef = icompile refProg

evalArr :: IO Int
evalArr = fmap evalExp $ interpret arrProg

compArr = icompile arrProg



type CMD2
    =   RefCMD Expr
    :+: ControlCMD Expr
    :+: FileCMD Expr

summer :: Program CMD2 ()
summer = do
    inp <- fopen "input" ReadMode
    let cont = fmap Not $ feof inp
    sum <- initRef (0 :: Expr Float)
    while cont $ do
        f <- fget inp
        s <- getRef sum
        setRef sum (s+f)
    s <- getRef sum
    printf "The sum is: %f\n" s

runSummer :: IO ()
runSummer = do
    writeFile "input" $ unwords $ map show ([-5..4] :: [Float])
    interpret summer

compSummer = icompile summer

