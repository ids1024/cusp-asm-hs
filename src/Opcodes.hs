module Opcodes (
    OpCodeOperand,
    OpCodeOperate
) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)

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
    fromEnum = fromJust . flip lookup opcodesOprTable
    toEnum = fromJust . flip lookup (map swap opcodesOprTable)

opcodesOprTable = group 0x00 [LDA, LDX, LDS, LDF, STA, STX, STS, STF,
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
    where group base ops = zip ops [base..]

data OpCodeOperate = TAX | TAS | TAF | TXA | TXS | TXF | TSA | TSX
                   | TSF | TFA | TFX | TFS | PSHA| PSHX| PSHF| POPA
                   | POPX| POPF
                   | NEGA| COMA| SHRA| SHLA| TLRA| RTLA| RROA| RLOA
                   | SOV | COV | SIE | CIE | SEQ | CEQ | SLT | CLT
                   | NOP | HLT
                   | RTN | IRTN
     deriving (Show, Read, Eq)

instance Enum OpCodeOperate where
    fromEnum = fromJust . flip lookup opcodesOpraTable
    toEnum = fromJust . flip lookup (map swap opcodesOpraTable)

opcodesOpraTable = group 0x00 [TAX, TAS, TAF, TXA, TXS, TXF, TSA, TSX,
                               TSF, TFA, TFX, TFS]
                ++ group 0x10 [PSHA, PSHX, PSHF, POPA, POPX, POPF]
                ++ group 0x20 [NEGA, COMA, SHRA, SHLA, TLRA, RTLA, RROA, RLOA]
                ++ group 0x30 [SOV, COV, SIE, CIE, SEQ, CEQ, SLT, CLT,
                               NOP]
                ++ group 0x40 [RTN, IRTN]
                ++ group 0xfff [HLT]
    where group base ops = zip ops [base..]
