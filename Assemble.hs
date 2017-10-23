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

-- Splits into lines of at most 6 words, in preparation for printing
--splitOps :: [(Int, Int)] -> [(Int, [Int])]
--splitOps [] = []
--splitOps ((n, val):rest) = case splitOps rest of
--    [] -> [(n, [val])]
--    (next_n, vals) : next_rest ->
--        if (next_n == n+1) && (length vals < 6)
--        then (n, val : vals) : next_rest
--        else (n, [val]) : (next_n, vals) : next_rest


splitOps :: [(Int, Int)] -> [(Int, [Int])]
--splitOps = splitOps_ 0 []

--splitOps_ _ _ [] = []
--splitOps_ prev_n line ((n, val) : ops) = 
--    if (n /= prev_n + 1) || (length line == 6)
--    then (prev_n, line) : splitOps_ n [val] ops
--    else splitOps_ n (line ++ [val]) ops

--splitOps ops = 
--    where (val, vals) = splitOps 0 ops
--
--splitOps_ _ _ [] = []
--splitOps_ prev_n count ((n, val) : ops) = 
--    if (n /= prev_n + 1) || (length count == 6)
--    then let (vals, rest) = splitOps_ n 0 ops
--         in ([], (val : vals) : rest)
--    else let (vals, rest) = splitOps_ n (count+1) ops
--         in (val : vals, rest)

splitOps = splitNonConsec
--splitNonConsec = snd . (splitNonConsec_ -1)
--splitNonConsec_ _ [] = ([], [])
--splitNonConsec_ prev_n ((n, val) : ops) =
--    if n == prev_n + 1
--    then ((val : vals), rest)
--    else ([], (n, val : vals) : rest)
--    where (vals, rest) = splitNonConsec_ n ops
--

splitNonConsec ops = (n, vals) : rest
    where (n, vals, rest) = splitNonConsec_ ops
splitNonConsec_ :: [(Int, Int)] -> (Int, [Int], [(Int, [Int])])
splitNonConsec_ [] = (0, [], [])
splitNonConsec_ ((n, val) : ops) =
    if next_n == n + 1
    then (n, val : next_vals, rest)
    else (n, [val], (next_n, next_vals) : rest)
    where (next_n, next_vals, rest) = splitNonConsec_ ops
