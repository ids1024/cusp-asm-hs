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
import Data.Tuple (swap)

data Operation = OpInstr Instruction | OpDir Directive | OpLabel String
     deriving Show

data OpCodeOperand = LDA | LDX | LDS | LDF | STA | STX | STS | STF
                   | PSH | POP | CLR | SET
                   | ADA | ADX | ADS | ADF | SBA | SBX | SBS | SBF
                   | MUL | DIV | MOD | INC | DEC | NEG
                   | CMA | CMX | CMS | CMF | TST
                   | AND | OR  | XOR | COM
                   | JMP | JSR | INT
                   | JEQ | JNE | JLT | JGE | JLE | JGT | GOV | JNO
                   | LDC | STC
                   | AOC | SOJ
                   | BGN | FIN
                   | INB | OUTB| INW | OUTW
     deriving (Show, Read, Eq)

-- https://stackoverflow.com/questions/6000511/better-way-to-define-an-enum-in-haskell
instance Enum OpCodeOperand where
    fromEnum = fromJust . flip lookup opcodes_opr_table
    toEnum = fromJust . flip lookup (map swap opcodes_opr_table)

opcodes_opr_table = group 0x00 [LDA, LDX, LDS, LDF, STA, STX, STS, STF,
                                PSH, POP, CLR, SET]
                 ++ group 0x10 [ADA, ADX, ADS, ADF, SBA, SBX, SBS, SBF,
                                MUL, DIV, MOD, INC, DEC, NEG]
                 ++ group 0x20 [CMA, CMX, CMS, CMF, TST]
                 ++ group 0x30 [AND, OR, XOR, COM]
                 ++ group 0x40 [JMP, JSR, INT]
                 ++ group 0x48 [JEQ, JNE, JLT, JGE, JLE, JGT, GOV, JNO]
                 ++ group 0x50 [LDC, STC]
                 ++ group 0x60 [AOC, SOJ]
                 ++ group 0x68 [BGN, FIN]
                 ++ group 0x70 [INB, OUTB, INW, OUTW]
    where group base ops = [(op, base + i) | (i, op) <- zip [0..] ops]

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
