module Instruction (
    Operation (..)
    , Instruction (..)
    , Directive (..)
    , Operand (..)
    , instr2word
    , opr2int
    , SymTable
    , symTableInsert
) where

import Data.Bits (shiftL, (.|.))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Control.Monad.Trans.State (State, get, put)

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

symTableInsert :: String -> Int -> State SymTable ()
symTableInsert name val = get >>= (put . Map.insert name val)

symTableLookup :: String -> State SymTable (Maybe Int)
symTableLookup name = Map.lookup name <$> get

instr2word :: Instruction -> State SymTable Int
instr2word (InstrOperand opcode mode addr) =
    do addr_int <- opr2int addr
       return $ fromEnum opcode `shiftL` 16
              + mode `shiftL` 12
              + addr_int
instr2word (InstrOperate opcode) = return $ 0xfff000 .|. fromEnum opcode

opr2int :: Operand -> State SymTable Int
opr2int opr = case opr of
    OprNum n -> return n
    OprName name -> fromJust <$> symTableLookup name
    OprAdd a b -> binary_op (+) a b
    OprSub a b -> binary_op (-) a b
    OprMul a b -> binary_op (*) a b
    where binary_op func a b = func <$> opr2int a <*> opr2int b
