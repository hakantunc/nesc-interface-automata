module Parser.Parser where

import Parser.Lexer
import Parser.Syntax
import System.Process
import Text.Parsec
import Text.Parsec.String

nescFile :: Parser NescFile
nescFile = 
     whiteSpace
  >> (I <$> interfaceDefinition <|> C <$> component)
  <* eof
-- 1

interfaceDefinition :: Parser InterfaceDefinition
interfaceDefinition
  = InterfaceDefinition
  <$  reserved "interface"
  <*> identifier
  <*> optionMaybe typeParameters
  <*> declarationList
-- 2

typeParameters :: Parser TypeParameters
typeParameters = angles . semiSep $ unwords <$> many identifier
-- 3 4

component :: Parser Component
component = do
  ck <- compKind
  Component ck
    <$> identifier
    <*> optionMaybe compParameters
    <*> optionMaybe attributes
    <*> componentSpecification
    <*> optionMaybe (implementation ck)
-- 5

compKind :: Parser CompKind
compKind = choice [
    Module               <$ reserved "module"
  , Configuration        <$ reserved "configuration"
  , GenericModule        <$ reserved "generic module"
  , GenericConfiguration <$ reserved "generic configuration"
  ]
-- 6

implementation :: CompKind -> Parser Implementation
implementation ck = case ck of
  Module               -> MI <$> moduleImplementation
  Configuration        -> CI <$> configurationImplementation
  GenericModule        -> MI <$> moduleImplementation
  GenericConfiguration -> CI <$> configurationImplementation
-- 7

compParameters :: Parser CompParameters
compParameters = CompParameters <$> parens (many (noneOf ")"))
-- 8

moduleImplementation :: Parser ModuleImplementation
moduleImplementation
  = reserved "implementation"
  *> many anyChar
-- 11

configurationImplementation :: Parser ConfigurationImplementation
configurationImplementation
  =  reserved "implementation"
  *> braces (configurationElement `endBy` semi)
-- 12 13

configurationElement :: Parser ConfigurationElement
configurationElement = choice [
    CC <$> components
  , CN <$> connection
  , CD <$> declaration
  ]
-- 14

components :: Parser Components
components = reserved "components" *>
  (componentLineSingle `sepBy` comma)
-- 15

componentLineSingle :: Parser ComponentLineSingle
componentLineSingle
  = ComponentLineSingle
  <$> componentRef
  <*> optionMaybe (reserved "as" *> identifier)
-- 16 17


componentRef :: Parser ComponentRef
componentRef = choice [
    CRI <$> identifier
  , reserved "new" *> (CRNew <$> identifier <*> parens componentArgumentList)
  -- , try $ CRI <$> identifier
  ]
-- 18

componentArgumentList :: Parser ComponentArgumentList
componentArgumentList = componentArgument `sepBy` comma
-- COMPLETE
-- 19

componentArgument :: Parser ComponentArgument
componentArgument = identifier
-- 20

connection :: Parser Connection
connection = choice
               [ f Equate "="
               , f LeftLink "<-"
               , f RightLink "->"
               ]
  where
    f x y = try $ x <$> endPoint <* symbol y <*> endPoint
-- 21

endPoint :: Parser EndPoint
endPoint = EndPoint
             <$> identifierPath
             <*> optionMaybe argumentExpressionList
-- 22

identifierPath :: Parser IdentifierPath
identifierPath = identifier `sepBy` dot
-- 23

componentSpecification :: Parser ComponentSpecification
componentSpecification = braces (many usesProvides)
-- 24 25

usesProvides :: Parser UsesProvides
usesProvides = choice [
    reserved "uses"     *> (Uses     <$> specificationElementList)
  , reserved "provides" *> (Provides <$> specificationElementList)
  ]
-- 26

specificationElementList :: Parser SpecificationElementList
specificationElementList = choice [
    SpecElem  <$> specificationElement
  , SpecElems <$> braces (many specificationElement)
  ]
-- 27 28
-- specificationElements :: Parser SpecificationElements
-- specificationElements = specificationElement `endBy` symbol ';'

specificationElement :: Parser SpecificationElement
specificationElement
  = SpecificationElement
  <$> (InterfaceType <$> (reserved "interface" *> identifier))
  <*> many (noneOf "{;}") <* semi
-- 29 30

attributes :: Parser Attributes
attributes = many attribute
-- 34

attribute :: Parser Attribute
attribute
  = Attribute
  <$> (symbol "@" *> identifier)
  <*> parens (many (noneOf ")"))
-- 35


-- # C. Imported Rules

argumentExpressionList :: Parser ArgumentExpressionList
argumentExpressionList = brackets (many (noneOf "]"))
-- C.1

declaration :: Parser Declaration
declaration = many . noneOf $ ";}"
-- C.3

declarationList :: Parser DeclarationList
declarationList = braces declarations
  where
    declarations = declaration `endBy` semi
-- C.4


parseNesc :: String -> IO (Either ParseError NescFile)
parseNesc fname = do
  input <- readProcess "sed" ["s/^#/\\/\\/#/", fname] ""
    -- comment out preprocessor directives
  return $ parse nescFile fname input
