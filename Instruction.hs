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

data Operation = OpInstr Instruction | OpDir Directive | OpLabel String
     deriving Show
-- UN* opcodes are placeholders
data OpCodeOperand = LDA | LDX | LDS | LDF | STA | STX | STS | STF
                   | PSH | POP | CLR | SET | UN0 | UN1 | UN2 | UN3
                   | ADA | ADX | ADS | ADF | SBA | SBX | SBS | SBF
                   | MUL | DIV | MOD | INC | DEC | NEG | UN4 | UN5
                   | CMA | CMX | CMS | CMF | TST | UN6 | UN7 | UN8
                   | AND | OR  | UN8a| UN8b| UN8c|UN8d | UN8e| UN8f
                   | XOR | COM | UN9 | UN10| UN11| UN12| UN12a|UN12b
                   | JMP | JSR | INT | UN13| UN14| UN15| UN16| UN17
                   | JEQ | JNE | JLT | JGE | JLE | JGT | GOV | JNO
                   | LDC | STC | UN18| UN19| UN20| UN21| UN22| UN23
                   | UN24| UN25| UN26| UN27| UN28| UN29| UN30| UN31
                   | AOC | SOJ | UN32| UN33| UN34| UN35| UN36| UN37
                   | BGN | FIN | UN38| UN39| UN40| UN41| UN42| UN43
                   | INB | OUTB| INW | OUTW
     deriving (Show, Read, Enum)
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
