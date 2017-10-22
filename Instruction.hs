module Instruction (
    Operation (OpInstr, OpDir, OpLabel)
    , Instruction (InstrOperate, InstrOperand)
    , Directive (DirEqu)
    , instr2word
    , SymTable
) where

import Data.Bits (shiftR, (.&.))
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)

data Operation = OpInstr Instruction | OpDir Directive | OpLabel String
     deriving Show
data Instruction = InstrOperand Int Int Symbol | InstrOperate Int
     deriving Show
data Symbol = SymNum Int | SymName BS.ByteString
     deriving Show
data Directive = DirEqu String
     deriving Show
type SymTable = Map.Map BS.ByteString Int

instr2word :: SymTable -> Instruction -> Int
instr2word symtable (InstrOperand opcode mode addr) =
           opcode `shiftR` 16 + mode `shiftR` 12 + sym2int symtable addr
instr2word _ (InstrOperate opcode) = 0xfff000 .&. opcode

sym2int :: SymTable -> Symbol -> Int
sym2int _ (SymNum n) = n
sym2int symtable (SymName name) = fromJust $ Map.lookup name symtable
