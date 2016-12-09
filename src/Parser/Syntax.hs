{-# LANGUAGE TemplateHaskell #-}
-- grammar specification for nesc and c
--   source: https://github.com/pillforge/nesc/blob/master/doc/ref.pdf

module Parser.Syntax where

import Control.Lens hiding ((<.>))

data NescFile
  = I {_i :: InterfaceDefinition}
  | C {_c :: Component}
  deriving (Eq, Show)
-- 1. nesC-file:
-- translation-unitopt interface-definition
-- translation-unitopt component

data InterfaceDefinition
  = InterfaceDefinition
    Identifier
    (Maybe TypeParameters)
    -- (Maybe Attributes)
    DeclarationList
  deriving (Eq, Show)
-- TODO 2. interface-definition:
-- interface identifier type-parametersopt attributesopt { declaration-list }

type TypeParameters = [String] -- comma separated strings
-- TODO 3. type-parameters:
-- < type-parameter-list >
-- 4. type-parameter-list:
-- identifier attributesopt
-- type-parameter-list , identifier attributesopt

data Component = Component {
    _componentKind :: CompKind
  , _componentName :: Identifier
  , _componentPara :: Maybe CompParameters
  , _componentAttr :: Maybe Attributes
  , _componentSpec :: ComponentSpecification
  , _componentImpl :: Maybe Implementation
  } deriving (Eq, Show)
-- 5. component:
-- comp-kind identifier comp-parametersopt attributesopt component-specification implementationopt

data CompKind
  = Module
  -- | Component -- never used
  | Configuration
  | GenericModule
  | GenericConfiguration
  deriving (Eq, Show)
-- 6. comp-kind:
-- module
-- component
-- configuration
-- generic module
-- generic configuration

data Implementation
  = MI {_mi :: ModuleImplementation}
  | CI {_ci :: ConfigurationImplementation}
  deriving (Eq, Show)
-- COMPLETE
-- 7. implementation:
-- module-implementation
-- configuration-implementation

newtype CompParameters
  = CompParameters 
    String
  deriving (Eq, Show)
-- TODO 8. comp-parameters:
-- ( component-parameter-list )

-- 9. component-parameter-list: component-parameter
-- component-parameter-list , component-parameter

-- 10. component-parameter: parameter-declaration
-- typedef identifier attributesopt

type ModuleImplementation = String
-- 11. module-implementation:
-- implementation { translation-unit }

type ConfigurationImplementation = [ConfigurationElement]
-- 12. configuration-implementation:
-- implementation { configuration-element-listopt }
-- 13. configuration-element-list:
-- configuration-element
-- configuration-element-list configuration-element

data ConfigurationElement
  = CC {_cc :: Components}
  | CN {_cn :: Connection}
  | CD {_cd :: Declaration}
  deriving (Eq, Show)
-- COMPLETE
-- 14.configuration-element:
-- components
-- connection
-- declaration

type Components = [ComponentLineSingle]
-- 15.components:
-- components component-line ;

data ComponentLineSingle = ComponentLineSingle {
    _compRef :: ComponentRef
  , _instNam :: Maybe Identifier
  } deriving (Eq, Show)
-- 16. component-line:
-- component-ref instance-nameopt
-- component-line , component-ref instance-nameopt
-- 17. instance-name:
-- as identifier

data ComponentRef
  = CRI {_cri :: Identifier}
  | CRNew {_crnew :: Identifier, _crnarg :: ComponentArgumentList}
  deriving (Eq, Show)
-- COMPLETE
-- 18.component-ref:
-- identifier
-- new identifier ( component-argument-list )

type ComponentArgumentList = [ComponentArgument]
-- COMPLETE
-- 19.component-argument-list:
-- component-argument
-- component-argument-list , component-argument

type ComponentArgument = Identifier
-- 20.component-argument:
-- expression
-- type-name

data Connection
  = Equate EndPoint EndPoint
  | LeftLink EndPoint EndPoint
  | RightLink EndPoint EndPoint
  deriving (Eq, Show)
-- 21.connection:
-- endpoint = endpoint
-- endpoint -> endpoint
-- endpoint <- endpoint

data EndPoint
  = EndPoint
    IdentifierPath
    (Maybe ArgumentExpressionList)
    deriving (Eq, Show)
-- 22.endpoint:
-- identifier-path
-- identifier-path [ argument-expression-list ]

type IdentifierPath = [Identifier]
-- 23.identifier-path:
-- identifier
-- identifier-path . identifier

type ComponentSpecification = [UsesProvides]
-- COMPLETE
-- 24.component-specification:
-- { uses-provides-list }
-- 25.uses-provides-list:
-- uses-provides
-- uses-provides-list uses-provides

data UsesProvides
  = Uses     {_uses'    :: SpecificationElementList}
  | Provides {_provides :: SpecificationElementList}
  deriving (Eq, Show)
-- 26.uses-provides:
-- uses specification-element-list
-- provides specification-element-list
-- declaration

data SpecificationElementList
  = SpecElem  { _specElem  :: SpecificationElement}
  | SpecElems { _specElems :: [SpecificationElement]}
  deriving (Eq, Show)
-- 27.specification-element-list:
-- specification-element
-- { specification-elements }
-- type SpecificationElements = [SpecificationElement]
-- 28.specification-elements:
-- specification-element
-- specification-elements specification-element

data SpecificationElement = SpecificationElement {
    _specElemIType :: InterfaceType
  , _unparsed      :: String
  } deriving (Eq, Show)
--   , _specElemIName :: Maybe InstanceName
--   , _specElemIPara :: Maybe InstanceParameters
--   , _specElemAttr  :: Maybe Attributes
-- 29. specification-element:
-- declaration
-- interface-type instance-nameopt instance-parametersopt attributesopt

data InterfaceType = InterfaceType {
    _interTypeIdent :: Identifier
  } deriving (Eq, Show)
-- 30.interface-type:
-- interface identifier type-argumentsopt

-- 31.type-arguments:
-- < type-argument-list >

-- 32.type-argument-list: type-name
-- type-argument-list , type-name

-- 33.instance-parameters:
-- [ parameter-type-list ]

type Attributes = [Attribute]
-- COMPLETE
-- 34.attributes:
-- attributes attribute
-- attribute

data Attribute
  = Attribute
    Identifier
    String
  deriving (Eq, Show)
-- 35.attribute:
-- @ identifier ( initializer-list )


-- # B. Changed rules:

-- B.1. typedef-name: also one of identifier . identifier

-- B.2. storage-class-specifier: also one of
-- command event async task norace

-- B.3. declaration-specifiers: also
-- default declaration-specifiers

-- B.4. direct-declarator: also
-- identifier . identifier
-- direct-declarator instance-parameters ( parameter-type-list )

-- B.5. struct-or-union-specifier: also one of
-- struct @ identifier attributes { struct-declaration-list } struct-or-union identifier attributes { struct-declaration-list }
-- attributes attribute

-- B.6. struct-or-union: also one of
-- nx struct nx union

-- B.7. enum-specifier: also one of
-- enum identifier attributes { enumerator-list }

-- B.8. init-declarator: also
-- declarator attributes
-- declarator attributes = initializer

-- B.9. struct-declarator: also declarator attributes
-- declarator : constant-expression attributes

-- B.10. parameter-declaration: also
-- declaration-specifiers declarator attributes

-- B.11. function-definition: also
-- declaration-specifiersopt declarator attributes declaration-listopt compound-statement

-- B.12. type-qualifier: also attribute

-- B.13. statement: also atomic-statement

-- B.14. atomic-statement:
-- atomic statement

-- B.15. postfix-expression: replaced by primary-expression
-- postfix-expression [ argument-expression-list ] call-kindopt primary ( argument-expression-listopt ) postfix-expression . identifier
-- postfix-expression -> identifier
-- postfix-expression ++
-- postfix-expression --

-- B.16. call-kind: one of
-- call signal post

-- # C. Imported Rules

type ArgumentExpressionList = String
-- C.1. argument-expression-list: A list of comma-separated expressions.

-- C.2. compound-stmt: A C { } block statement.

type Declaration = String
-- C.3. declaration: A C declaration.

type DeclarationList = [Declaration]
-- C.4. declaration-list: A list of C declarations.

-- C.5. declaration-specifiers: A list of storage classes, type specifiers and type qualifiers.

-- C.6. declarator : The part of a C declaration that specifies the array, function and pointer parts of
-- the declared entity’s type.

-- C.7. direct-declarator : Like declarator, but with no leading pointer-type specification.

-- C.8. enumerator-list: List of constant declarations inside an enum.

-- C.9. expression: Any C expression.

type Identifier = String
-- C.10. identifier : Any C identifier.

-- C.11. init-declarator : The part of a C declaration that specifies the array, function and pointer
-- parts of the declared entity’s type, and its initialiser (if any).

-- C.12. initializer : An initializer for a variable declaration.

-- C.13. initializer-list: An initializer for a compound type without the enclosing {, kw}.

-- C.14. parameter-declaration: A function parameter declaration.

-- C.15. parameter-type-list: Specification of a function’s parameters.

-- C.16. postfix-expression: A restricted class of C expressions.

-- C.17. primary: An identifier, constant, string or parenthesised expression.

-- C.18. statement: Any C statement.

-- C.19. storage-class-specifier : A storage class specification for a C declaration.

-- C.20. struct-declaration-list: Declarations inside a struct or union.

-- C.21. translation-unit: A list of C declarations and function definitions.

-- C.22. type-name: A C type specification.


makeLenses ''NescFile                  -- 1
makeLenses ''Component                 -- 5
makeLenses ''Implementation            -- 7
makeLenses ''ConfigurationElement      -- 14
makeLenses ''ComponentLineSingle       -- 16 17
makeLenses ''ComponentRef              -- 18
makeLenses ''UsesProvides              -- 26
makeLenses ''SpecificationElementList  -- 27
makeLenses ''SpecificationElement      -- 29
makeLenses ''InterfaceType             -- 30
