-- based on http://jakewheat.github.io/intro_to_parsing
-- grammar specification is at https://github.com/pillforge/nesc/blob/master/doc/ref.pdf

import Control.Monad
import System.Environment
import Text.Parsec
import Text.Parsec.String

data InterfaceDefinition = InterfaceDefinition
  InterfaceKeyword Identifier TypeParameters DeclarationList deriving (Eq, Show)
data InterfaceKeyword = Interface deriving (Eq, Show)
type Identifier = String
type TypeParameters = [Identifier]
type DeclarationList = [String]

interfaceDefinition :: Parser InterfaceDefinition
interfaceDefinition = InterfaceDefinition
                        <$> interface
                        <*> identifier
                        <*> option [] typeParameters
                        <*> declarationList
                        <*  eof

interface :: Parser InterfaceKeyword
interface = Interface <$ lexeme (string "interface")

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

typeParameters :: Parser TypeParameters
typeParameters = between (symbol '<') (symbol '>') parameters
  where
    parameters = identifier `sepBy` symbol ','

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
  choice [ simpleWhitespace       *> whitespace
         , lineComment            *> whitespace
         , blockComment           *> whitespace
         , preprocessorDirectives *> whitespace
         , return ()]
  where
    simpleWhitespace       = void $ many1 (oneOf " \t\n")
    lineComment            = try (string "//")
                               *> manyTill anyChar (void (char '\n') <|> eof)
    blockComment           = try (string "/*")
                               *> manyTill anyChar (try $ string "*/")
    preprocessorDirectives = try (string "#")
                               *> manyTill anyChar (void (char '\n') <|> eof)

myParser :: Parser InterfaceDefinition
myParser = interfaceDefinition

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseFromFile myParser str >>= either print print
    _ -> error "please pass one argument with the file containing the text to parse"
