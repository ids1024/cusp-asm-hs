module Instruction (
    Instruction (InstrOperate, InstrOperand)
    , instr2word
    , SymTable
) where

import Data.Bits (shiftR, (.&.))
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)

data Instruction = InstrOperand Int Int Symbol | InstrOperate Int
data Symbol = SymNum Int | SymName BS.ByteString
type SymTable = Map.Map BS.ByteString Int

instr2word :: SymTable -> Instruction -> Int
instr2word symtable (InstrOperand opcode mode addr) =
           opcode `shiftR` 16 + mode `shiftR` 12 + sym2int symtable addr
instr2word _ (InstrOperate opcode) = 0xfff000 .&. opcode

sym2int :: SymTable -> Symbol -> Int
sym2int _ (SymNum n) = n
sym2int symtable (SymName name) = fromJust $ Map.lookup name symtable
