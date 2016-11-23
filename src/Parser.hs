{-# LANGUAGE TemplateHaskell #-}
-- based on http://jakewheat.github.io/intro_to_parsing
-- grammar specification for nesc and c
--   source: https://github.com/pillforge/nesc/blob/master/doc/ref.pdf

module Parser where

import Control.Lens hiding (noneOf)
import Control.Monad
import System.Environment
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as T
import Data.Functor.Identity

data NescFile = I {_i :: InterfaceDefinition}
              | C {_c :: Component}
              deriving (Eq, Show)

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

data Component = Component {
    _compType :: CompKind
  , _compName :: Identifier
  , _compPara :: Maybe CompParameters
  , _compAttr :: Maybe Attributes
  , _compSpec :: ComponentSpecification
  , _compImpl :: Implementation
  } deriving (Eq, Show)

data CompKind = Module | ComponentKeyword | Configuration | GenericModule | GenericConfiguration
                  deriving (Eq, Show)
type CompParameters = String
type Attributes = String

type ComponentSpecification = [UsesProvides]
data UsesProvides = Uses     {_uses     :: SpecificationElementList}
                  | Provides {_provides :: SpecificationElementList}
                  deriving (Eq, Show)

data SpecificationElementList = SpecElem  { _specElem  :: SpecificationElement}
                              | SpecElems { _specElems :: SpecificationElements}
                              deriving (Eq, Show)

type SpecificationElements = [SpecificationElement]

type SpecificationElement = String
-- data SpecificationElement = SpecificationElement {
--     _specElemIType :: InterfaceType
--   , _specElemIName :: Maybe InstanceName
--   , _specElemIPara :: Maybe InstanceParameters
--   , _specElemAttr  :: Maybe Attributes
--   } deriving (Eq, Show)

data InterfaceType = InterfaceType {
    _iTypeIdentifier :: Identifier
  , _iTypeArguments  :: Maybe TypeArguments
  } deriving (Eq, Show)
type TypeArguments = [TypeName]
type TypeName = String

type InstanceParameters = ParameterTypeList


data Implementation = CI {_ci :: ConfigurationImplementation}
                    | MI {_mi :: ModuleImplementation}
                    deriving (Eq, Show)

type ConfigurationImplementation = ConfigurationElementList
type ConfigurationElementList = [ConfigurationElement]

data ConfigurationElement = CC {_cc :: Components}
                          | CN {_cn :: Connection}
                          | CD {_cd :: Declaration}
                          deriving (Eq, Show)

type Components = [ComponentLine]

data ComponentLine = ComponentLine {
    _compRef :: ComponentRef
  , _instNam :: InstanceName
  } deriving (Eq, Show)

data ComponentRef = CRI {_cri :: Identifier}
                  | CRNew {_crnew :: Identifier, _crnarg :: ComponentArgumentList}
                  deriving (Eq, Show)

type ComponentArgumentList = [ComponentArgument]
type InstanceName = Identifier
type ComponentArgument = Identifier
data Connection = Equate EndPoint EndPoint
                | LeftLink EndPoint EndPoint
                | RightLink EndPoint EndPoint deriving (Eq, Show)

data EndPoint = EndPoint
                  IdentifierPath
                  (Maybe ArgumentExpressionList)
                  deriving (Eq, Show)
type IdentifierPath = [Identifier]
type ArgumentExpressionList = String

type ModuleImplementation = String

makeLenses ''NescFile
makeLenses ''Component
makeLenses ''ConfigurationElement
makeLenses ''ComponentLine
makeLenses ''ComponentRef
makeLenses ''Implementation

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
              <*> optionMaybe compParameters
              <*> optionMaybe attributes
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

compParameters :: Parser CompParameters
compParameters = between (symbol '(') (symbol ')') (many (noneOf ")"))

attributes :: Parser Attributes
attributes = many (noneOf "{")

componentSpecification :: Parser ComponentSpecification
componentSpecification = between (symbol '{') (symbol '}') (many usesProvides)

usesProvides :: Parser UsesProvides
usesProvides = choice
                 [ trylexemestr "uses" *> (Uses <$> specificationElementList)
                 , trylexemestr "provides" *> (Provides <$> specificationElementList)
                 ]

specificationElementList :: Parser SpecificationElementList
specificationElementList = choice
                             [ try $ SpecElems <$> between (symbol '{') (symbol '}') specificationElements
                             , SpecElem <$> specificationElement <* char ';'
                             ]

specificationElements :: Parser SpecificationElements
specificationElements = specificationElement `endBy` symbol ';'

specificationElement :: Parser SpecificationElement
specificationElement = many (noneOf ";")
-- trylexemestr "interface" *>
--                          (SpecificationElement
--                           <$> interfaceType
--                           <*> optionMaybe identifier
--                           <*> optionMaybe instanceParameters
--                           <*> optionMaybe identifier)

interfaceType :: Parser InterfaceType
interfaceType = InterfaceType
                  <$> identifier
                  <*> optionMaybe typeArguments

typeArguments :: Parser TypeArguments
typeArguments = between (symbol '<') (symbol '>') (many (many (noneOf "]")))

instanceParameters :: Parser InstanceParameters
instanceParameters = between (symbol '[') (symbol ']') parameterTypeList

implementation :: Parser Implementation
implementation = trylexemestr "implementation" *>
                  choice
                    [ try $ CI <$> between (symbol '{') (symbol '}') configurationImplementation
                    , MI <$> moduleImplementation
                    ]

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
componentLine = ComponentLine <$> componentRef <*> option [] instanceName

componentRef :: Parser ComponentRef
componentRef = choice
                 [ trylexemestr "new" *> (CRNew <$> identifier <*> componentArgumentList)
                 , try $ CRI <$> identifier
                 ]

componentArgumentList :: Parser ComponentArgumentList
componentArgumentList = between (symbol '(') (symbol ')') (componentArgument `sepBy` symbol ',')

instanceName :: Parser InstanceName
instanceName = lexeme (string "as") *> identifier

componentArgument :: Parser ComponentArgument
componentArgument = identifier

connection :: Parser Connection
connection = choice
               [ f Equate "="
               , f LeftLink "<-"
               , f RightLink "->"
               ]
  where
    f x y = try $ x <$> endPoint <* trylexemestr y <*> endPoint

endPoint :: Parser EndPoint
endPoint = EndPoint
             <$> identifierPath
             <*> optionMaybe argumentExpressionList

identifierPath :: Parser IdentifierPath
identifierPath = identifier `sepBy` symbol '.'

argumentExpressionList :: Parser ArgumentExpressionList
argumentExpressionList = between (symbol '[') (symbol ']') (many (noneOf "]"))

moduleImplementation :: Parser ModuleImplementation
moduleImplementation = many anyChar

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
