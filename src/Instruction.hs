module Instruction (
    Operation (..)
    , Instruction (..)
    , Directive (..)
    , Operand (..)
    , instr2word
    , opr2int
    , SymTable
) where

import Data.Bits (shiftL, (.|.))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Opcodes (OpCodeOperand, OpCodeOperate)

data Operation = OpInstr Instruction | OpDir Directive | OpLabel String
     deriving Show

data Instruction = InstrOperand OpCodeOperand Int Operand
                 | InstrOperate OpCodeOperate
     deriving Show

data Operand = OprNum Int | OprName String | OprAdd Operand Operand 
             | OprSub Operand Operand | OprMul Operand Operand
     deriving Show

data Directive = DirEqu String Int | DirWord Operand | DirBlkw Operand
     deriving Show

type SymTable = Map.Map String Int

instr2word :: SymTable -> Instruction -> Int
instr2word symtable (InstrOperand opcode mode addr) =
           (fromEnum opcode) `shiftL` 16
         + mode `shiftL` 12
         + opr2int symtable addr
instr2word _ (InstrOperate opcode) = 0xfff000 .|. (fromEnum opcode)

opr2int :: SymTable -> Operand -> Int
opr2int symtable opr = case opr of
    OprNum n -> n
    OprName name -> fromJust $ Map.lookup name symtable
    OprAdd a b -> binary_op (+) a b
    OprSub a b -> binary_op (-) a b
    OprMul a b -> binary_op (*) a b
    where binary_op func a b = (opr2int symtable a) `func` (opr2int symtable b)
