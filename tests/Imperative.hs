{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for various constructs

module Imperative where



import Data.Int
import Data.Word
import System.Directory
import System.FilePath
import System.Process
import System.Random

import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import Language.Embedded.CExp



type CMD
    =   RefCMD
    :+: ArrCMD
    :+: ControlCMD
    :+: PtrCMD
    :+: FileCMD
    :+: C_CMD

type Prog = Program CMD (Param2 CExp CType)

prog :: Prog ()
prog = do
    r <- initRef (10 :: CExp Int32)
    a <- getRef r
    modifyRef r (*a)
    printf "%d\n" a



-- | Test primitive types
testTypes :: Prog ()
testTypes = do
    inp :: CExp Int32 <- fget stdin
    a <- unsafeFreezeRef =<< initRef (i2n inp + 0x88               :: CExp Int8)
    b <- unsafeFreezeRef =<< initRef (i2n inp + 0x8888             :: CExp Int16)
    c <- unsafeFreezeRef =<< initRef (i2n inp + 0x88888888         :: CExp Int32)
    d <- unsafeFreezeRef =<< initRef (i2n inp + 0x8888888888888888 :: CExp Int64)
    e <- unsafeFreezeRef =<< initRef (i2n inp + 0xEE               :: CExp Word8)
    f <- unsafeFreezeRef =<< initRef (i2n inp + 0xEEEE             :: CExp Word16)
    g <- unsafeFreezeRef =<< initRef (i2n inp + 0xEEEEEEEE         :: CExp Word32)
    h <- unsafeFreezeRef =<< initRef (i2n inp + 0xEEEEEEEEEEEEEEEE :: CExp Word64)
    i <- unsafeFreezeRef =<< initRef (i2n inp - 9                  :: CExp Float)
    j <- unsafeFreezeRef =<< initRef (i2n inp - 10                 :: CExp Double)
    printf "%d %d %d %ld %u %u %u %lu %.3f %.3f\n" a b c d e f g h i j
    k1 <- unsafeFreezeRef =<< initRef true
    k2 <- unsafeFreezeRef =<< initRef true
    iff ((k1 #&& k2) #|| not_ k1) (printf "true") (printf "false")

testCExp :: Prog ()
testCExp = do
    a :: CExp Int32 <- fget stdin
    let b = a#==10 ? a*3 $ a-5+8
    let c = i2n a/23 :: CExp Double
    printf "%d " b
    printf "%d " (not_ (a#==10) ? a*3 $ a-5+8)
    printf "%d " (a `quot_` b)
    printf "%d " (a #% b)
    printf "%d " (cond (i2b a) a b)
    printf "%d " (b2i (not_ (a#==10)) * a)
    printf "%.3f " c
    printf "%.3f " (i2n a :: CExp Float)

testRef :: Prog ()
testRef = do
    r1 <- newRef
    r2 <- initRef (3 :: CExp Int32)
    modifyRef r2 (*2)
    setRef r1 =<< getRef r2
    a <- unsafeFreezeRef r1
    b <- unsafeFreezeRef r2
    printf "%d %d\n" a b

testCopyArr1 :: Prog ()
testCopyArr1 = do
    arr1 :: Arr Word32 Int32 <- newArr (10 :: CExp Word32)
    arr2 :: Arr Word32 Int32 <- newArr (10 :: CExp Word32)
    sequence_ [setArr arr1 i (i2n i+10) | i' <- [0..9], let i = fromInteger i']
    copyArr (arr2,0) (arr1,0) 10
    sequence_ [getArr arr2 i >>= printf "%d " . (*3) | i' <- [0..9], let i = fromInteger i']
    printf "\n"

testCopyArr2 :: Prog ()
testCopyArr2 = do
    arr1 :: Arr Word32 Int32 <- newArr (20 :: CExp Word32)
    arr2 :: Arr Word32 Int32 <- newArr (20 :: CExp Word32)
    sequence_ [setArr arr1 i (i2n i+10) | i' <- [0..19], let i = fromInteger i']
    copyArr (arr2,10) (arr1,5) 10
    sequence_ [getArr arr2 i >>= printf "%d " . (*3) | i' <- [10..19], let i = fromInteger i']
    printf "\n"

testArr2 :: Prog ()
testArr2 = do
    n <- fget stdin
    arr :: Arr Word32 Int32 <- newArr n  -- Array of dynamic length
    sequence_ [setArr arr (i2n i) i | i' <- [0..3], let i = fromInteger i']
    sequence_ [getArr arr i >>= printf "%d " . (*3) | i' <- [0..3], let i = fromInteger i']
    printf "\n"
    return ()

testArr3 :: Prog ()
testArr3 = do
    arr :: Arr Word32 Int32 <- constArr [8,7,6,5]
    sequence_ [getArr arr i >>= printf "%d " . (*3) | i' <- [0..3], let i = fromInteger i']
    printf "\n"
    return ()

testArr4 :: Prog ()
testArr4 = do
    arr :: Arr Word32 Int32 <- constArr [8,7,6,5]
    iarr <- freezeArr arr 4
    sequence_ [printf "%d " $ iarr #! i | i' <- [0..3], let i = fromInteger i']
    printf "\n"

testArr5 :: Prog ()
testArr5 = do
    arr :: Arr Word32 Int32 <- constArr [8,7,6,5]
    iarr <- unsafeFreezeArr arr
    sequence_ [printf "%d " $ iarr #! i | i' <- [0..3], let i = fromInteger i']
    printf "\n"

testArr6 :: Prog ()
testArr6 = do
    arr :: Arr Word32 Int32 <- constArr [8,7,6,5]
    iarr <- unsafeFreezeArr arr
    arr2 <- unsafeThawArr iarr
    sequence_ [getArr arr2 i >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"

testArr7 :: Prog ()
testArr7 = do
    arr :: Arr Word32 Int32 <- constArr [8,7,6,5]
    iarr <- freezeArr arr 4
    arr2 <- thawArr iarr 4
    sequence_ [getArr arr2 i >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"

testSwap1 :: Prog ()
testSwap1 = do
    arr1 :: Arr Word32 Int32 <- constArr [1,2,3,4]
    arr2 :: Arr Word32 Int32 <- constArr [11,12,13,14]
    unsafeSwap arr1 arr2
    sequence_ [getArr arr1 i >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"

testSwap2 :: Prog ()
testSwap2 = do
    arr1 :: Arr Word32 Int32 <- constArr [1,2,3,4]
    n <- fget stdin
    arr2 :: Arr Word32 Int32 <- newArr n
    copyArr (arr2,0) (arr1,0) 4
    setArr arr2 2 22
    unsafeSwap arr1 arr2
    sequence_ [getArr arr1 i >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"
    sequence_ [getArr arr2 i >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"

testIf1 :: Prog ()
testIf1 = do
    inp :: CExp Int32 <- fget stdin
    a <- ifE (inp #== 10)        (return (inp+1)) (return (inp*3))
    b <- ifE (not_ (inp #== 10)) (return (a+1))   (return (a*3))
    printf "%d %d\n" a b

testIf2 :: Prog ()
testIf2 = do
    inp :: CExp Int32 <- fget stdin
    iff (inp #== 11)        (printf "== 11\n") (printf "/= 11\n")
    iff (not_ (inp #== 11)) (printf "/= 11\n") (printf "== 11\n")
    iff (inp #== 12)        (printf "== 12\n") (return ())
    iff (not_ (inp #== 12)) (return ())        (printf "== 12\n")
    iff (inp #== 13)        (printf "== 13\n") (return ())
    iff (not_ (inp #== 13)) (return ())        (printf "== 13\n")
    iff (inp #== 14)        (return ())        (return ())

-- Loop from 0 to 9 in steps of 1
testFor1 :: Prog ()
testFor1 = for (0,1,9) $ \i ->
    printf "%d\n" (i :: CExp Int8)

-- Loop from 9 to 0 in steps of 2
testFor2 :: Prog ()
testFor2 = for (9,-2,0) $ \i ->
    printf "%d\n" (i :: CExp Int8)

-- Loop from 0 to but excluding 10 in steps of 2
testFor3 :: Prog ()
testFor3 = for (0, 2, Excl 10) $ \i ->
    printf "%d\n" (i :: CExp Int8)

-- While loop tested in `sumInput` in Demo.hs.

testAssert :: Prog ()
testAssert = do
    inp :: CExp Int32 <- fget stdin
    assert (inp #> 0) "input too small"
    printf "past assertion\n"

-- This tests that `formatSpecifier` works as it should for different types
testPrintScan :: (Formattable a, CType a) => CExp a -> Prog ()
testPrintScan a = do
    i <- fget stdin
    fput stdout "" (i `asTypeOf` a) ""

testPtr :: Prog ()
testPtr = do
    addInclude "<stdlib.h>"
    addInclude "<string.h>"
    addInclude "<stdio.h>"
    p :: Ptr Int32 <- newPtr
    callProcAssign p "malloc" [valArg (100 :: CExp Word32)]
    arr :: Arr Word32 Int32 <- constArr [34,45,56,67,78]
    callProc "memcpy" [ptrArg p, arrArg arr, valArg (5*4 :: CExp Word32)]  -- sizeof(int32_t) = 4
    callProc "printf" [strArg "%d\n", deref $ ptrArg p]
    iarr :: IArr Word32 Int32 <- unsafeFreezeArr =<< ptrToArr p
    printf "sum: %d\n" (iarr#!0 + iarr#!1 + iarr#!2 + iarr#!3 + iarr#!4)
    callProc "free" [ptrArg p]

testArgs :: Prog ()
testArgs = do
    addInclude "<stdio.h>"
    addInclude "<stdbool.h>"
    addDefinition setPtr_def
    addDefinition ret_def
    let v = 55 :: CExp Int32
    r <- initRef (66 :: CExp Int32)
    a :: Arr Int32 Int32 <- constArr [234..300]
    ia <- freezeArr a 10
    p :: Ptr Int32 <- newPtr
    o <- newObject "int" False
    op <- newObject "int" True
    callProcAssign p "setPtr" [refArg r]
    callProcAssign o "ret" [valArg v]
    callProcAssign op "setPtr" [refArg r]
    callProc "printf"
        [ strArg "%d %d %d %d %d %d %d %d %d %d\n"
        , valArg v
        , deref (refArg r)
        , deref (arrArg a)
        , deref (iarrArg ia)
        , deref (ptrArg p)
        , deref (offset (iarrArg ia) (3 :: CExp Word32))
        , deref (offset (ptrArg p) (0 :: CExp Word32))
        , objArg o
        , deref (objArg op)
        , constArg "bool" "true"
        ]
  where
    setPtr_def = [cedecl|
        int * setPtr (int *a) {
            return a;
        }
        |]
    ret_def = [cedecl|
        int ret (int a) {
            return a;
        }
        |]

testExternArgs :: Prog ()
testExternArgs = do
    addInclude "<stdbool.h>"
    let v = 55 :: CExp Int32
    externProc "val_proc1" [valArg v]
    externProc "val_proc2" [offset3 $ valArg v]
      -- Normal integer addition (slight misuse of `offset`)
    _ :: CExp Int32 <- externFun "val_fun" [valArg v]
    r <- initRef v
    externProc "ref_proc1" [refArg r]
    externProc "ref_proc2" [deref $ refArg r]  -- TODO Simplify
    a :: Arr Int32 Int32 <- newArr 10
    externProc "arr_proc1" [arrArg a]
    externProc "arr_proc2" [addr $ arrArg a]
    externProc "arr_proc3" [deref $ arrArg a]
    externProc "arr_proc4" [offset3 $ arrArg a]
    externProc "arr_proc5" [deref $ offset3 $ arrArg a]
    externProc "arr_proc6" [offsetMinus $ arrArg a]
    p :: Ptr Int32 <- newPtr
    externProc "ptr_proc1" [ptrArg p]
    externProc "ptr_proc2" [addr $ ptrArg p]
    externProc "ptr_proc3" [deref $ ptrArg p]
    o <- newObject "int" False
    externProc "obj_proc1" [objArg o]
    externProc "obj_proc2" [addr $ objArg o]
    op <- newObject "int" True
    externProc "obj_proc3" [objArg op]
    externProc "obj_proc4" [addr $ objArg op]
    externProc "obj_proc5" [deref $ objArg op]
    externProc "obj_proc6" [offset3 $ objArg op]
    let s = "apa"
    externProc "str_proc1" [strArg s]
    externProc "str_proc2" [deref $ strArg s]
    externProc "const_proc" [constArg "bool" "true"]
    return ()
  where
    offset3     = flip offset (3 :: CExp Int32)
    offsetMinus = flip offset (-3 :: CExp Int32) . offset3

testCallFun :: Prog ()
testCallFun = do
    addInclude "<math.h>"
    i :: CExp Int32 <- fget stdin
    a <- callFun "sin" [valArg (i2n i :: CExp Double)]
    printf "%.3f\n" (a :: CExp Double)

multiModule :: Prog ()
multiModule = do
    addInclude "<stdlib.h>"
    addExternProc "func_in_other" []
    inModule "other" $ do
      addDefinition [cedecl|
        void func_in_other(void) {
          puts("Hello from the other module!");
        } |]
      addInclude "<stdio.h>"
    callProc "func_in_other" []

testMultiModule :: IO ()
testMultiModule = do
    tmp <- getTemporaryDirectory
    rand <- randomRIO (1, maxBound :: Int)
    let temp = tmp </> "imperative-edsl_" ++ show rand
    exists <- doesDirectoryExist temp
    when exists $ removeDirectoryRecursive temp
    createDirectory temp
    let ms    = compileAll multiModule
        files = [temp </> "imperative-edsl_" ++ m ++ ".c" | (m,_) <- ms]
        exe   = temp </> "imperative-edsl"
        cmd   = unwords $ ("cc -o" : exe : files)
    zipWithM_ writeFile files (map snd ms)
    putStrLn cmd
    system cmd
    putStrLn exe
    system exe
    exists <- doesDirectoryExist temp
    when exists $ removeDirectoryRecursive temp



----------------------------------------

-- It would be nice to be able to run these tests using Tests.Tasty.HUnit, but
-- I wasn't able to make that work, probably due to the use of `fakeIO` in the
-- tests. First, Tasty wasn't able to silence the output of the tests, and
-- secondly, the tests would always fail when running a second time.

testAll = do
    tag "testTypes"    >> compareCompiled  testTypes    (runIO testTypes)                      "0\n"
    tag "testCExp"     >> compareCompiledM testCExp     (runIO testCExp)                       "44\n"
    tag "testRef"      >> compareCompiled  testRef      (runIO testRef)                        ""
    tag "testCopyArr1" >> compareCompiled  testCopyArr1 (runIO testCopyArr1)                   ""
    tag "testCopyArr2" >> compareCompiled  testCopyArr2 (runIO testCopyArr2)                   ""
    tag "testArr2"     >> compareCompiled  testArr2     (runIO testArr2)                       "20\n"
    tag "testArr3"     >> compareCompiled  testArr3     (runIO testArr3)                       ""
    tag "testArr4"     >> compareCompiled  testArr4     (runIO testArr4)                       ""
    tag "testArr5"     >> compareCompiled  testArr5     (runIO testArr5)                       ""
    tag "testArr6"     >> compareCompiled  testArr6     (runIO testArr6)                       ""
    tag "testArr7"     >> compareCompiled  testArr7     (runIO testArr6)                       ""
    tag "testArr7"     >> compareCompiled  testArr7     (runIO testArr7)                       ""
    tag "testSwap1"    >> compareCompiled  testSwap1    (runIO testSwap1)                      ""
    tag "testSwap2"    >> compareCompiled  testSwap2    (runIO testSwap2)                      "45\n"
    tag "testIf1"      >> compareCompiled  testIf1      (runIO testIf1)                        "12\n"
    tag "testIf2"      >> compareCompiled  testIf2      (runIO testIf2)                        "12\n"
    tag "testFor1"     >> compareCompiled  testFor1     (runIO testFor1)                       ""
    tag "testFor2"     >> compareCompiled  testFor2     (runIO testFor2)                       ""
    tag "testFor3"     >> compareCompiled  testFor3     (runIO testFor3)                       ""
    tag "testAssert"   >> compareCompiled  testAssert   (runIO testAssert)                     "45"
    tag "testPtr"      >> compareCompiled  testPtr      (putStrLn "34" >> putStrLn "sum: 280") ""
    tag "testArgs"     >> compareCompiled  testArgs     (putStrLn "55 66 234 234 66 237 66 55 66 1")  ""

    tag "testPrintScan_Int8"   >> compareCompiled (testPrintScan int8)   (runIO (testPrintScan int8))   "45"
    tag "testPrintScan_Int16"  >> compareCompiled (testPrintScan int16)  (runIO (testPrintScan int16))  "45"
    tag "testPrintScan_Int32"  >> compareCompiled (testPrintScan int32)  (runIO (testPrintScan int32))  "45"
    tag "testPrintScan_Int64"  >> compareCompiled (testPrintScan int64)  (runIO (testPrintScan int64))  "45"
    tag "testPrintScan_Word8"  >> compareCompiled (testPrintScan word8)  (runIO (testPrintScan word8))  "45"
    tag "testPrintScan_Word16" >> compareCompiled (testPrintScan word16) (runIO (testPrintScan word16)) "45"
    tag "testPrintScan_Word32" >> compareCompiled (testPrintScan word32) (runIO (testPrintScan word32)) "45"
    tag "testPrintScan_Word64" >> compareCompiled (testPrintScan word64) (runIO (testPrintScan word64)) "45"
    tag "testPrintScan_Float"  >> captureCompiled (testPrintScan float)  "45"
    tag "testPrintScan_Double" >> captureCompiled (testPrintScan double) "45"
      -- `testPrintScan` for floating point types can't be compared to `runIO`,
      -- becuase different number of digits are printed

    tag "testExternArgs" >> compileAndCheck testExternArgs
    tag "testCallFun" >> compareCompiledM testCallFun (putStrLn "-0.757") "4"
    tag "multiModule" >> testMultiModule
  where
    tag str = putStrLn $ "---------------- tests/Imperative.hs/" ++ str ++ "\n"
    compareCompiledM = compareCompiled' def {externalFlagsPost = ["-lm"]}

    int8   = 0 :: CExp Int8
    int16  = 0 :: CExp Int16
    int32  = 0 :: CExp Int32
    int64  = 0 :: CExp Int64
    word8  = 0 :: CExp Word8
    word16 = 0 :: CExp Word16
    word32 = 0 :: CExp Word32
    word64 = 0 :: CExp Word64
    float  = 0 :: CExp Float
    double = 0 :: CExp Double

