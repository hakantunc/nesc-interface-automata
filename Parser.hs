-- based on http://jakewheat.github.io/intro_to_parsing

import Control.Monad
import System.Environment
import Text.Parsec
import Text.Parsec.String

data InterfaceDefinition = InterfaceDefinition
  InterfaceKeyword Identifier DeclarationList deriving (Eq, Show)
data InterfaceKeyword = Interface deriving (Eq, Show)
type Identifier = String
type DeclarationList = [String]

interfaceDefinition :: Parser InterfaceDefinition
interfaceDefinition = InterfaceDefinition
                        <$> interface
                        <*> identifier
                        <*> declarationList
                        <*  eof

interface :: Parser InterfaceKeyword
interface = Interface <$ lexeme (string "interface")

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

declarationList :: Parser DeclarationList
declarationList = between (symbol '{') (symbol '}') declarations
  where
    declarations = declaration `endBy` symbol ';'
    declaration = many (noneOf ";}")

lexeme :: Parser a -> Parser a
lexeme p = whitespace >> p <* whitespace

symbol :: Char -> Parser Char
symbol c = lexeme $ char c

whitespace :: Parser ()
whitespace =
  choice [ simpleWhitespace *> whitespace
         , lineComment *> whitespace
         , blockComment *> whitespace
         , return ()]
  where
    lineComment = try (string "//")
                  *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment = try (string "/*")
                   *> manyTill anyChar (try $ string "*/")
    simpleWhitespace = void $ many1 (oneOf " \t\n")

myParser :: Parser InterfaceDefinition
myParser = interfaceDefinition

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseFromFile myParser str >>= either print print
    _ -> error "please pass one argument with the file containing the text to parse"
