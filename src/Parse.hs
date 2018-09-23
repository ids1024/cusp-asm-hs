{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-do-bind #-}

module Parse (parseAsm) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (maybeToList, fromJust, catMaybes, isJust)
import Data.Bits ((.|.))
import Data.Void
import Data.Char (toUpper)
import Numeric (readHex)
import Instruction (Operation (..)
                  , Directive (..)
                  , Operand (..)
                  , Instruction (..))


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

operand = try oprmul <|> try opradd <|> try oprsub
      <|> try oprname <|> oprnum
          where oprname = OprName <$> identifier
                oprnum = OprNum <$> num
                binary_op c op = do a <- try oprname <|> oprnum
                                    whitespace
                                    char c
                                    whitespace
                                    b <- operand
                                    return $ op a b
                opradd = binary_op '+' OprAdd
                oprsub = binary_op '-' OprSub
                oprmul = binary_op '*' OprMul -- TODO: order of operations

line :: Parser [Operation]
line = do whitespace
          l <- optional (try label_)
          whitespace
          op <- optional (directive <|> instruction)
          whitespace
          optional comment
          return $ catMaybes [l, op]

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

dirblkw = do string' "blkw"
             whitespace1
             val <- operand
             return (DirBlkw val)

directive = do char '.'
               dir <- direqu <|> dirword <|> dirblkw
               return (OpDir dir)

-- XXX other address modes
addressingMode = do c <- optional $ oneOf (map fst mode_map)
                    whitespace1
                    frame <- optional $ char '!'
                    let val = maybe def (fromJust . flip lookup mode_map) c
                    let frame_val = if isJust frame then 1 else 0
                    return $ val  .|. frame_val
                 where mode_map = [('#', 0), ('+', 4), ('*', 6), ('&', 8)]
                       def = 2

operandInstruction = do instr <- some letterChar
                        mode <- addressingMode
                        whitespace
                        oper <- operand
                        let opcode = read (map toUpper instr)
                        return $ InstrOperand opcode mode oper

operateInstruction = do instr <- some letterChar
                        let opcode = read (map toUpper instr)
                        return $ InstrOperate opcode

instruction = do instr <- try operandInstruction <|> operateInstruction
                 return $ OpInstr instr

parseAsm :: String -> Either (ParseError Char Void) [Operation]
parseAsm = parse asmFile "(unknown)"
