module Assemble (assemble) where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import Data.Void
import qualified Data.Map as Map
import Text.Megaparsec (ParseError)

import Instruction (Operation(..), Instruction (..), Directive(..), Operand(..), SymTable, instr2word)
import Parse (parseAsm)

assemble :: String -> Either (ParseError Char Void) String
assemble code = case parseAsm code of
    Left err -> Left err
    Right ops -> Right $ show $ pass2 $ pass1 ops

pass1 :: [Operation] -> (SymTable, [(Int, Operation)])
pass1 = pass1_ Map.empty 0

pass1_ :: SymTable -> Int -> [Operation] -> (SymTable, [(Int, Operation)])
pass1_ symtable _ [] = (symtable, [])
pass1_ symtable loc (op:ops) = case op of
    OpInstr instr -> let (symtable, res) = pass1_ symtable (loc+1) ops
                     in (symtable, (loc, op) : res)
    OpDir dir -> case dir of
        DirEqu ident val ->
            if ident == "@"
            then pass1_ symtable val ops
            else pass1_ (Map.insert ident val symtable) loc ops
        DirWord val -> let (symtable, res) = pass1_ symtable (loc+1) ops
                       in (symtable, (loc, op) : res)
    OpLabel label -> pass1_ (Map.insert label loc symtable) loc ops

pass2 :: (SymTable, [(Int, Operation)]) -> [(Int, Int)]
pass2 (_, []) = []
pass2 (symtable, (pos, op):ops) = (pos, word) : pass2 (symtable, ops)
    where word = case op of
                      OpInstr instr -> instr2word symtable instr
                      OpDir (DirWord val) -> val
                      otherwise -> error "Unexpected"
