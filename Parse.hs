module Parse (parse) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators
import Data.Maybe (maybeToList)
import Data.Void
import Instruction (Operation (OpInstr, OpDir, OpLabel)
                   , Directive (DirEqu)
                   ,Instruction (InstrOperate, InstrOperand))


type Parser = Parsec Void String

asmFile :: Parser [Operation]
asmFile = do ops <- sepBy line eol
             eof
             return $ (foldr (++) []) ops

whitespace = many $ oneOf " \t"
identifier = do a <- letterChar
                b <- many alphaNumChar
                return $ a : b

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

directive = do char '.'
               -- XXX
               return (OpDir $ DirEqu "")

instruction :: Parser Operation
instruction = do instr <- some letterChar
                 mode <- optional (char '#')
                 whitespace
                 operand <- optional (alphaNumChar <|> (char '$'))
                 operand <- optional $ some alphaNumChar
                 return (OpInstr $ InstrOperate 1) -- XXX

--parseAsm :: String -> Either (ParseError Char Void) [Operation]
--parseAsm = parse asmFile "(unknown)"

main :: IO ()
main =  getContents >>= parseTest asmFile
