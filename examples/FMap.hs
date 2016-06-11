module FMap where

import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import Language.Embedded.CExp


type CMD
    =   RefCMD
    :+: ControlCMD
    :+: C_CMD

type Prog = Program CMD (Param2 CExp CType)

example :: Prog ()
example = do
  ptr <- offload "0x83C00000"

  assignp ptr 1 10
  assignp ptr 2 5

  v <- loadp ptr 0

  closep

test = icompile example
