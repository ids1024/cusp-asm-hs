module Instruction (
    Operation (OpInstr, OpDir, OpLabel)
    , Instruction (InstrOperate, InstrOperand)
    , Directive (DirEqu)
    , Operand (OprNum, OprName)
    , instr2word
    , SymTable
) where

import Data.Bits (shiftL, (.&.))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Opcodes (OpCodeOperand, OpCodeOperate)

data Operation = OpInstr Instruction | OpDir Directive | OpLabel String
     deriving Show

data Instruction = InstrOperand OpCodeOperand Int Operand
                 | InstrOperate OpCodeOperate
     deriving Show

data Operand = OprNum Int | OprName String
     deriving Show

data Directive = DirEqu String
     deriving Show

type SymTable = Map.Map String Int

instr2word :: SymTable -> Instruction -> Int
instr2word symtable (InstrOperand opcode mode addr) =
           (fromEnum opcode) `shiftL` 16
         + mode `shiftL` 12
         + opr2int symtable addr
instr2word _ (InstrOperate opcode) = 0xfff000 .&. (fromEnum opcode)

opr2int :: SymTable -> Operand -> Int
opr2int _ (OprNum n) = n
opr2int symtable (OprName name) = fromJust $ Map.lookup name symtable
