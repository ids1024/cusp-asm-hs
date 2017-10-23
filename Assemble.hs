module Assemble (assemble) where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import Data.Void
import Data.List (sortOn, intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import Text.Printf (printf)

import Text.Megaparsec (ParseError)
import Data.List.Split (chunksOf)

import Instruction (Operation(..), Instruction (..), Directive(..), Operand(..), SymTable, instr2word)
import Parse (parseAsm)

assemble :: String -> Either (ParseError Char Void) String
assemble code = case parseAsm code of
    Left err -> Left err
    Right ops -> Right $ toText $ pass2 $ pass1 ops

pass1 :: [Operation] -> (SymTable, [(Int, Operation)])
pass1 ops = (symtable, sortOn fst res)
    where (symtable, res) = pass1_ Map.empty 0 ops

pass1_ :: SymTable -> Int -> [Operation] -> (SymTable, [(Int, Operation)])
pass1_ symtable _ [] = (symtable, [])
pass1_ symtable loc (op:ops) = case op of
    OpInstr instr -> let (new_symtable, res) = pass1_ symtable (loc+1) ops
                     in (new_symtable, (loc, op) : res)
    OpDir (DirEqu ident val) ->
        if ident == "@"
        then pass1_ symtable val ops
        else pass1_ (Map.insert ident val symtable) loc ops
    OpDir (DirWord val) -> let (new_symtable, res) = pass1_ symtable (loc+1) ops
                           in (new_symtable, (loc, op) : res)
    OpLabel label -> pass1_ (Map.insert label loc symtable) loc ops

pass2 :: (SymTable, [(Int, Operation)]) -> [(Int, Int)]
pass2 (_, []) = []
pass2 (symtable, ((pos, op):ops)) = (pos, word) : pass2 (symtable, ops)
    where word = case op of
                      OpInstr instr -> instr2word symtable instr
                      OpDir (DirWord val) -> val
                      otherwise -> error "Unexpected"

toText :: [(Int, Int)] -> String
toText = (++"\n") . (intercalate "\n") . (map line) . splitOps
    where line (n, vals) = printf "$%03X  " n 
                        ++ intercalate "  " (map chunk (chunksOf 4 vals))
          chunk = intercalate " " . (map value)
          value val = let s = printf "%06X" val
                      -- For truncating two's complement negatives
                      -- XXX making architecture assumption?
                      in '$' : drop (length s - 6) s

-- Splits into lines of at most 8 words, in preparation for printing
splitOps :: [(Int, Int)] -> [(Int, [Int])]
splitOps = concat . (map splitLine) . splitConsec
    where splitLine (n, vals) = zip [n,n+8..] (chunksOf 8 vals)

-- Split into chunks of consecutive words
splitConsec [] = []
splitConsec ((n, val) : ops) =
    case splitConsec ops of
        [] -> [(n, [val])]
        (next_n, next_vals) : rest ->
            if next_n == n + 1
            then (n, val : next_vals) : rest
            else (n, [val]) : (next_n, next_vals) : rest
