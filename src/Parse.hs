module Parse (parseAsm) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators
import Data.Maybe (maybeToList, fromJust, catMaybes)
import Data.Void
import Data.Char (toUpper)
import Numeric (readHex)
import Instruction (Operation (OpInstr, OpDir, OpLabel)
                   , Directive (DirEqu, DirWord)
                   , Operand (OprNum, OprName, OprAdd, OprSub)
                   ,Instruction (InstrOperate, InstrOperand))


type Parser = Parsec Void String

asmFile :: Parser [Operation]
asmFile = do ops <- sepBy line eol
             eof
             return $ concat ops

whitespace = many $ oneOf " \t"
whitespace1 = some $ oneOf " \t"
identifier = do a <- letterChar <|> char '_'
                b <- many $ alphaNumChar <|> char '_'
                return $ a : b

num = try num10 <|> num16
      where num10 = do neg <- optional (char '-')
                       n <- some numberChar
                       return $ read (maybeToList neg ++ n)
            num16 = do char '$'
                       n <- some hexDigitChar
                       return $ fst $ head $ readHex n

operand = (try opradd) <|> (try oprsub) <|> (try oprname) <|> oprnum
          where oprname = identifier >>= (return . OprName)
                oprnum = num >>= (return . OprNum)
                opradd = do a <- (try oprname) <|> oprnum
                            whitespace
                            char '+'
                            whitespace
                            b <- operand
                            return $ OprAdd a b
                oprsub = do a <- (try oprname) <|> oprnum
                            whitespace
                            char '-'
                            whitespace
                            b <- operand
                            return $ OprSub a b

line :: Parser [Operation]
line = do whitespace
          label <- optional (try label_)
          whitespace
          op <- optional (directive <|> instruction)
          whitespace
          optional comment
          return $ catMaybes [label, op]

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
             val <- operand
             return (DirWord val)

directive = do char '.'
               dir <- direqu <|> dirword
               return (OpDir dir)

-- XXX other address modes
addressing_mode = do c <- oneOf (map fst mode_map)
                     return $ fromJust $ lookup c mode_map
                  where mode_map = [('#', 0), (' ', 2), ('\t', 2), ('+', 4)]

operand_instruction = do instr <- some letterChar
                         mode <- addressing_mode
                         whitespace
                         oper <- operand
                         let opcode = read (map toUpper instr)
                         return $ InstrOperand opcode mode oper

operate_instruction = do instr <- some letterChar
                         let opcode = read (map toUpper instr)
                         return $ InstrOperate opcode

instruction = do instr <- (try operand_instruction) <|> operate_instruction
                 return $ OpInstr instr

parseAsm :: String -> Either (ParseError Char Void) [Operation]
parseAsm = parse asmFile "(unknown)"
