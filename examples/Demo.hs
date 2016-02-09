{-# LANGUAGE TypeOperators #-}

module Demo where



import Control.Applicative ((<$>))
import Data.Word

import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import Language.Embedded.CExp



-- | Custom instruction type with: references, control structures and file I/O
type CMD
    =   RefCMD CExp
    :+: ControlCMD CExp
    :+: FileCMD CExp

-- | Program that asks the user for numbers and prints their sum
sumInput :: Program CMD ()
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

run_sumInput = runCompiled sumInput



testAll = do
    compareCompiled sumInput (unlines $ map show $ reverse [0..20]) (interpret sumInput)

