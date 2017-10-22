module Parse (parse) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators
import Data.Maybe (maybeToList, isJust)
import Data.Void
import Data.Char (toUpper)
import Numeric (readHex)
import Instruction (Operation (OpInstr, OpDir, OpLabel)
                   , Directive (DirEqu, DirWord)
                   , Operand (OprNum, OprName)
                   ,Instruction (InstrOperate, InstrOperand))


type Parser = Parsec Void String

asmFile :: Parser [Operation]
asmFile = do ops <- sepBy line eol
             eof
             return $ (foldr (++) []) ops

whitespace = many $ oneOf " \t"
whitespace1 = some $ oneOf " \t"
identifier = do a <- letterChar
                b <- many alphaNumChar
                return $ a : b

num = try num10 <|> num16
      where num10 = do neg <- optional (char '-')
                       n <- some numberChar
                       return $ read (maybeToList neg ++ n)
            num16 = do char '$'
                       n <- some hexDigitChar
                       return $ fst $ head $ readHex n

operand = (try oprname) <|> oprnum
          where oprname = identifier >>= (return . OprName)
                oprnum = num >>= (return . OprNum)

line :: Parser [Operation]
line = do whitespace
          label <- optional (try label_)
          whitespace
          op <- optional (directive <|> instruction)
          whitespace
          optional comment
          return $ (maybeToList label) ++ (maybeToList op)

comment = do char ';'
             many $ noneOf "\n"

label_ = do symbol <- identifier
            char ':'
            return (OpLabel symbol)

direqu = do string' "equ"
            whitespace1 
            ident <- try identifier <|> string "@"
            char ','
            val <- num
            return (DirEqu ident val)

dirword = do string' "word"
             whitespace1
             val <- num
             return (DirWord val)

directive = do char '.'
               dir <- direqu <|> dirword
               return (OpDir dir)

operand_instruction = do instr <- some letterChar
                         mode <- optional (char '#')
                         whitespace1
                         oper <- operand
                         let opcode = read (map toUpper instr)
                         -- XXX other address modes
                         let mode_num = if isJust mode then 0 else 2
                         return $ InstrOperand opcode mode_num oper

operate_instruction = do instr <- some letterChar
                         let opcode = read (map toUpper instr)
                         return $ InstrOperate opcode

instruction = do instr <- (try operand_instruction) <|> operate_instruction
                 return $ OpInstr instr

--parseAsm :: String -> Either (ParseError Char Void) [Operation]
--parseAsm = parse asmFile "(unknown)"

main :: IO ()
main =  getContents >>= parseTest asmFile
