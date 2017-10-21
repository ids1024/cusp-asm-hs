module Assemble (assemble) where

import Instruction (Instruction, SymTable, instr2word)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map

assemble :: BS.ByteString -> BS.ByteString
assemble code = pass2 $ pass1 code

pass1 :: BS.ByteString -> (SymTable, [Instruction])
pass1 code = (SymTable, 
              map parseLine $ filter (not . null) $ map splitLine $ C.lines code)
    where splitLine = C.words . C.takeWhile (/= ';')
          parseLines lines = parseLines_ SymTable 0 lines
	  parseLines_ symtable _ [] = (symtable, [])
          parseLines_  symtable instr_ctr (words : lines)
	      | C.last (head words) == ':' =
	          -- Label
	          let symtable = Map.insert (C.tail $ head words) instr_ctr symtable
		  in parseLines_ symtable instr_ctr (tail words : lines)
	      | head words == C.pack ".equ" =
	          1
	      | C.head (head words) == '.' =
	          error "unrecognized compiler directive"
	      | otherwise =
	          1

pass2 :: SymTable -> [Instruction] -> BS.ByteString
pass2 symtable instructions = BS.pack bytes
     where bytes = foldr (++) [] $ map word2bytes words
           words = map (instr2word symtable) instructions

word2bytes :: Int -> [Word8]
word2bytes word = map (fromIntegral . (.&.) 0xff)
                      [word `shiftR` 16, word `shiftR` 8, word]
