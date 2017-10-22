module Instruction (
    Operation (OpInstr, OpDir, OpLabel)
    , Instruction (InstrOperate, InstrOperand)
    , Directive (DirEqu)
    , Operand (OprNum, OprName)
    , instr2word
    , SymTable
) where

import Data.Bits (shiftR, (.&.))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Opcodes (OpCodeOperand)

data Operation = OpInstr Instruction | OpDir Directive | OpLabel String
     deriving Show

data Instruction = InstrOperand Int Int Operand | InstrOperate Int
     deriving Show

data Operand = OprNum Int | OprName String
     deriving Show

data Directive = DirEqu String
     deriving Show

type SymTable = Map.Map String Int

instr2word :: SymTable -> Instruction -> Int
instr2word symtable (InstrOperand opcode mode addr) =
           opcode `shiftR` 16 + mode `shiftR` 12 + opr2int symtable addr
instr2word _ (InstrOperate opcode) = 0xfff000 .&. opcode

opr2int :: SymTable -> Operand -> Int
opr2int _ (OprNum n) = n
opr2int symtable (OprName name) = fromJust $ Map.lookup name symtable

main :: IO ()
main = do line <- getLine
          print $ fromEnum ((read line) :: OpCodeOperand)
