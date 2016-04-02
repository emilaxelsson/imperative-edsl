{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module C where

import Prelude hiding (break)

import Language.C.Quote.C
import Language.Embedded.Imperative
import Language.Embedded.Concurrent
import Language.Embedded.Backend.C
import Language.Embedded.CExp

type L =
  C_CMD CExp :+:
  FileCMD CExp

-- | Define a function in another module and call it.
multiModule :: Program L ()
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

----------------------------------------

testAll = do
    icompileAll multiModule

--------------------------------------------------------------------------------

memmap
  :: String -- Address
  -> [Ptr a]
  -> Program L ()
memmap addr ptrs =
  do output <- newPtr :: Program L (Ptr Int8)
     offset <- newPtr :: Program L (Ptr Int8)
     addExternProc "mem_map" [strArg addr, ptrArg output, ptrArg offset]
     inModule "mem" $ do
       addDefinition [cedecl|
         int mem_map(unsigned addr, void **ptr, unsigned *offset) {
           unsigned page_size = sysconf(_SC_PAGESIZE);
           int mem_fd = open("/dev/mem", O_RDWR);
           if (mem_fd < 1) {
             return -1;
           }
           unsigned page_addr = (addr & (~(page_size-1)));
           *offset = addr - page_addr;
           *ptr = mmap(NULL, page_size, PROT_READ|PROT_WRITE, MAP_SHARED, mem_fd, page_addr);
           if (*ptr == MAP_FAILED || !*ptr) {
             return -2;
           }
           return 0;
         } |]

co :: Program L ()
co = do
  
  memmap "0x83C00000" []

----------------------------------------

testMem = do
    icompileAll co

----------------------------------------
