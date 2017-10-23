module Assemble (assemble) where

import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import Data.Void
import Data.List (sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import Text.Megaparsec (ParseError)

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
toText = show . splitOps

splitOps :: [(Int, Int)] -> [(Int, [Int])]
--splitOps = splitOps_loc 0

--splitOps_loc loc ops = maybeToList first ++ rest
--                       where (first, rest) = splitOps_ loc 0 ops

--splitOps_ _ _ [] = (Nothing, [])
--splitOps_ loc line_len (((n, val):rest))
--    | (loc /= n) || (line_len == 6) = (Just (loc, [val]), splitOps_loc (loc+1) rest)
--    | otherwise = (Just (loc, val : cur), next_rest)
--                  where (next, next_rest) = splitOps_ (loc+1) (line_len+1) rest
--                        (_, cur) = fromMaybe (loc, []) next
--
splitOps [] = []
splitOps ((n, val):rest) = case splitOps rest of
    [] -> [(n, [val])]
    (next_n, vals) : next_rest ->
        if (next_n == n+1) && (length vals < 6)
        then (n, val : vals) : next_rest
        else (n, [val]) : (next_n, vals) : next_rest
