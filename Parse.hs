module Parse (parse) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Instruction (Operation (OpInstr, OpLabel)
                   ,Instruction (InstrOperate, InstrOperand))

asmFile :: GenParser Char st [Operation]
asmFile = endBy line endOfLine

line = do comment
          return $ OpInstr $ InstrOperate 1

comment = do char ';'
             many anyChar

label :: GenParser Char st Operation
label = do char '.'
           symbol <- many letter
           return (OpLabel symbol)

parseAsm :: String -> Either ParseError [Operation]
parseAsm = parse asmFile "(unknown)"

main :: IO ()
main = interact (show . parseAsm)
