module Assemble (assemble) where

import Data.Void
import Data.List (sortOn, intercalate)
import qualified Data.Map as Map
import Data.Bifunctor (second)
import Text.Printf (printf)
import Control.Monad ((>=>))
import Control.Monad.Trans.State (State, evalState)

import Text.Megaparsec (ParseError)
import Data.List.Split (chunksOf)

import Instruction (Operation(..), Directive(..), SymTable, instr2word, opr2int, symTableInsert)
import Parse (parseAsm)

assemble :: String -> Either (ParseError Char Void) String
assemble = second (toText . (`evalState` Map.empty) . (pass1 >=> pass2)) . parseAsm

-- First pass of assembly. Turns list of operations to a symbol table and a
-- list of address/operation pairs. The output will not include operations
-- like labels that do not take space in memory.
pass1 :: [Operation] -> State SymTable [(Int, Operation)]
pass1 = fmap (sortOn fst) . pass1_ 0

pass1_ :: Int -> [Operation] -> State SymTable [(Int, Operation)]
pass1_ _ [] = return []
pass1_ loc (op:ops) = case op of
    OpInstr _ -> ((loc, op) :) <$> pass1_ (loc+1) ops
    OpDir (DirEqu "@" val) -> pass1_ val ops
    OpDir (DirEqu ident val) -> symTableInsert ident val >> pass1_ loc ops
    OpDir (DirWord _) -> ((loc, op) :) <$> pass1_ (loc+1) ops
    OpDir (DirBlkw opr) -> opr2int opr >>= flip pass1_ ops
    OpLabel label -> symTableInsert label loc >> pass1_ loc ops

-- Second pass of assembly. Resolves symbols and instructions to their
-- numerical values
pass2 :: [(Int, Operation)] -> State SymTable [(Int, Int)]
pass2 [] = return []
pass2 ((pos, op):ops) = curry (:) pos <$> word <*> pass2 ops
    where word = case op of
                 OpInstr instr -> instr2word instr
                 OpDir (DirWord val) -> opr2int val
                 _ -> error "Unexpected"

toText :: [(Int, Int)] -> String
toText = unlines . map line . splitOps
    where line (n, vals) = printf "$%03X  " n 
                        ++ intercalate "  " (map chunk (chunksOf 4 vals))
          chunk = unwords . map value
          value val = let s = printf "%06X" val
                      -- For truncating two's complement negatives
                      -- XXX making architecture assumption?
                      in '$' : drop (length s - 6) s

-- Splits into lines of at most 8 words, in preparation for printing
splitOps :: [(Int, Int)] -> [(Int, [Int])]
splitOps = concatMap splitLine . splitConsec
    where splitLine (n, vals) = zip [n,n+8..] (chunksOf 8 vals)

-- Split into chunks of consecutive words
splitConsec :: [(Int, Int)] -> [(Int, [Int])]
splitConsec [] = []
splitConsec ((n, val) : ops) =
    case splitConsec ops of
        (next_n, next_vals) : rest | next_n == n+1 ->
            (n, val : next_vals) : rest
        rest ->
            (n, [val]) : rest
