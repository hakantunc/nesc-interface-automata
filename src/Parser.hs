-- based on http://jakewheat.github.io/intro_to_parsing
-- grammar specification for nesc and c
--   source: https://github.com/pillforge/nesc/blob/master/doc/ref.pdf

module Parser where

import Control.Monad
import System.Environment
import Text.Parsec
import Text.Parsec.String
import Data.Functor.Identity

data NescFile = I InterfaceDefinition | C Component deriving (Eq, Show)
data InterfaceDefinition = InterfaceDefinition
  InterfaceKeyword Identifier TypeParameters DeclarationList deriving (Eq, Show)
data InterfaceKeyword = Interface deriving (Eq, Show)
type Identifier = String
type TypeParameters = [Identifier]
type DeclarationList = [Declaration]
data Declaration = Declaration DeclarationSpecifiers InitDeclaratorList deriving (Eq, Show)
type DeclarationSpecifiers = [DeclarationSpecifier]
data DeclarationSpecifier = SCS StorageClassSpecifier | TS TypeSpecifier deriving (Eq, Show)
type InitDeclaratorList = [InitDeclarator]
data InitDeclarator = InitDeclarator Declarator deriving (Eq, Show)
type Declarator = DirectDeclarator
data DirectDeclarator = DDPTL Identifier ParameterTypeList deriving (Eq, Show)
type ParameterTypeList = [ParameterList]
type ParameterList = ParameterDeclaration
data ParameterDeclaration = PDDSD DeclarationSpecifier Identifier deriving (Eq, Show)
data StorageClassSpecifier = Auto | Register | Static | Extern | Typedef
                           | Command | Event | Async | Task | Norace deriving (Enum, Eq, Show)
data TypeSpecifier = Void | Char | Short | Int | Long
                   | Float | Double | Signed | Unsigned
                   | Int8_t | Int16_t | Int32_t | Int64_t
                   | Uint8_t | Uint16_t | Uint32_t | Uint64_t
                   | Bool
                   -- | TypedefName Identifier -- hard to handle, check page 234 in K&R.
                   deriving (Eq, Show)
data Component = Component
  CompKind Identifier ComponentSpecification Implementation deriving (Eq, Show)
data CompKind = Module | ComponentKeyword | Configuration | GenericModule | GenericConfiguration
                  deriving (Eq, Show)
type ComponentSpecification = String
type Implementation = ConfigurationImplementation
type ConfigurationImplementation = ConfigurationElementList
type ConfigurationElementList = [ConfigurationElement]
data ConfigurationElement = CC Components | CN Connection | CD Declaration deriving (Eq, Show)
type Components = [ComponentLine]
data ComponentLine = ComponentLine ComponentRef InstanceName deriving (Eq, Show)
data ComponentRef = CRI Identifier | CRNew Identifier ComponentArgumentList deriving (Eq, Show)
type ComponentArgumentList = [ComponentArgument]
type InstanceName = Identifier
type ComponentArgument = Identifier
data Connection = Equate EndPoint EndPoint
                | LeftLink EndPoint EndPoint
                | RightLink EndPoint EndPoint deriving (Eq, Show)
type EndPoint = IdentifierPath
type IdentifierPath = [Identifier]

nescFile :: Parser NescFile
nescFile = choice
             [ try $ I <$> interfaceDefinition
             , C <$> component
             ]

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

declaration :: Parser Declaration
declaration = Declaration <$> declarationSpecifiers <*> initDeclaratorList

declarationSpecifiers :: Parser DeclarationSpecifiers
declarationSpecifiers = many declarationSpecifier

declarationSpecifier :: Parser DeclarationSpecifier
declarationSpecifier = choice
                         [ SCS <$> storageClassSpecifier
                         , TS  <$> typeSpecifier]

initDeclaratorList :: Parser InitDeclaratorList
initDeclaratorList = many initDeclarator

initDeclarator :: Parser InitDeclarator
initDeclarator = InitDeclarator <$> declarator

declarator :: Parser Declarator
declarator = directDeclarator

directDeclarator :: Parser DirectDeclarator
directDeclarator = DDPTL <$> identifier <*> between (symbol '(') (symbol ')') parameterTypeList

parameterTypeList :: Parser ParameterTypeList
parameterTypeList = parameterList `sepBy` symbol ','

parameterList :: Parser ParameterList
parameterList = parameterDeclaration

parameterDeclaration :: Parser ParameterDeclaration
parameterDeclaration = PDDSD <$> declarationSpecifier <*> identifier

storageClassSpecifier :: Parser StorageClassSpecifier
storageClassSpecifier = choice
                          [ Auto     <$ trylexemestr "auto"
                          , Register <$ trylexemestr "register"
                          , Static   <$ trylexemestr "static"
                          , Extern   <$ trylexemestr "extern"
                          , Typedef  <$ trylexemestr "typedef"
                          , Command  <$ trylexemestr "command"
                          , Event    <$ trylexemestr "event"
                          , Async    <$ trylexemestr "async"
                          , Task     <$ trylexemestr "task"
                          , Norace   <$ trylexemestr "norace"]

typeSpecifier :: Parser TypeSpecifier
typeSpecifier = choice
                  [ Void        <$ trylexemestr "void"
                  , Char        <$ trylexemestr "char"
                  , Short       <$ trylexemestr "short"
                  , Int         <$ trylexemestr "int"
                  , Long        <$ trylexemestr "long"
                  , Float       <$ trylexemestr "float"
                  , Double      <$ trylexemestr "double"
                  , Signed      <$ trylexemestr "signed"
                  , Unsigned    <$ trylexemestr "unsigned"
                  -- , TypedefName <$> identifier
                  , Int8_t      <$ trylexemestr "int8_t"
                  , Int16_t     <$ trylexemestr "int16_t"
                  , Int32_t     <$ trylexemestr "int32_t"
                  , Int64_t     <$ trylexemestr "int64_t"
                  , Uint8_t     <$ trylexemestr "uint8_t"
                  , Uint16_t    <$ trylexemestr "uint16_t"
                  , Uint32_t    <$ trylexemestr "uint32_t"
                  , Uint64_t    <$ trylexemestr "uint64_t"
                  , Bool        <$ trylexemestr "bool"
                  ]

component :: Parser Component
component = Component
              <$> compKind
              <*> identifier
              <*> componentSpecification
              <*> implementation

compKind :: Parser CompKind
compKind = choice
             [ Module               <$ trylexemestr "module"
             , ComponentKeyword     <$ trylexemestr "component"
             , Configuration        <$ trylexemestr "configuration"
             , GenericModule        <$ trylexemestr "generic module"
             , GenericConfiguration <$ trylexemestr "generic configuration"
             ]

componentSpecification :: Parser ComponentSpecification
componentSpecification = between (symbol '{') (symbol '}') (string "")

implementation :: Parser Implementation
implementation = trylexemestr "implementation" *>
                   between (symbol '{') (symbol '}') configurationImplementation

configurationImplementation :: Parser ConfigurationImplementation
configurationImplementation = configurationElementList

configurationElementList :: Parser ConfigurationElementList
configurationElementList = configurationElement `endBy` symbol ';'

configurationElement :: Parser ConfigurationElement
configurationElement = choice
                         [ CC <$> components
                         , CN <$> connection
                         , CD <$> declaration
                         ]

components :: Parser Components
components = trylexemestr "components" *> (componentLine `sepBy` symbol ',')

componentLine :: Parser ComponentLine
componentLine = ComponentLine <$> componentRef <*> (option [] instanceName)

componentRef :: Parser ComponentRef
componentRef = choice
                 [ trylexemestr "new" *> (CRNew <$> identifier <*> componentArgumentList)
                 , try $ CRI <$> identifier
                 ]

componentArgumentList :: Parser ComponentArgumentList
componentArgumentList = between (symbol '(') (symbol ')') (componentArgument `sepBy` symbol ',')

instanceName :: Parser InstanceName
instanceName = (lexeme $ string "as") *> identifier

componentArgument :: Parser ComponentArgument
componentArgument = identifier

connection :: Parser Connection
connection = choice
               [ f Equate "="
               , f LeftLink "<-"
               , f RightLink "->"
               ]
  where
    f x y = try $ x <$> identifierPath <* (trylexemestr y) <*> identifierPath

endPoint :: Parser EndPoint
endPoint = identifierPath

identifierPath :: Parser IdentifierPath
identifierPath = identifier `sepBy` symbol '.'

lexeme :: Parser a -> Parser a
lexeme p = whitespace >> p <* whitespace

symbol :: Char -> Parser Char
symbol c = lexeme $ char c

trylexemestr :: String -> ParsecT String () Data.Functor.Identity.Identity String
trylexemestr = try . lexeme . string

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

parseNesc :: String -> IO (Either ParseError NescFile)
parseNesc = parseFromFile nescFile
