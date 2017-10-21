module Assemble (assemble) where

import Instruction (Instruction, SymTable, instr2word)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

assemble :: BS.ByteString -> BS.ByteString
assemble code = pass2 $ pass1 code

pass1 :: BS.ByteString -> (SymTable, [Instruction])
pass1 code = (SymTable, 
              map parseLine $ filter (not . null) $ map splitLine $ C.lines code)
    where splitLine = C.words . C.takeWhile (/= ';')
          parseLine words
	      | C.last (head words) == ':' =
	          -- Label
	          1
	      | C.head (head words) == '.' =
	          -- Compiler directive
	          1
	      | otherwise =
	          1

pass2 :: SymTable -> [Instruction] -> BS.ByteString
pass2 symtable instructions = BS.pack bytes
     where bytes = foldr (++) [] $ map word2bytes words
           words = map (instr2word symtable) instructions

word2bytes :: Int -> [Word8]
word2bytes word = map (fromIntegral . (.&.) 0xff)
                      [word `shiftR` 16, word `shiftR` 8, word]
