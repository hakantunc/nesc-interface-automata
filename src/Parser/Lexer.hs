module Parser.Lexer where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

reservedNames :: [String]
reservedNames =
  [ "as"
  , "atomic"
  , "async"
  , "call"
  , "command"
  , "component"
  , "components"
  , "configuration"
  , "event"
  , "generic"
  , "implementation"
  , "includes"
  , "interface"
  , "module"
  , "new"
  , "norace"
  , "nx_struct"
  , "nx_union"
  , "post"
  , "provides"
  , "signal"
  , "task"
  , "uses"
  , "abstract"
  , "extends"
  , "generic module"
  , "generic configuration"
  ]

nescDef :: P.LanguageDef st
nescDef = emptyDef {
    P.commentStart   = "/*"
  , P.commentEnd     = "*/"
  , P.commentLine    = "//"
  , P.nestedComments = True
  , P.identStart     = letter
  , P.identLetter    = alphaNum <|> oneOf "_'"
  , P.reservedNames  = reservedNames
  , P.reservedOpNames= []
  , P.caseSensitive  = False
  }

lexer         = P.makeTokenParser nescDef
identifier    = P.identifier lexer
reserved      = P.reserved lexer
charLiteral   = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer
symbol        = P.symbol lexer
lexeme        = P.lexeme lexer
whiteSpace    = P.whiteSpace lexer
parens        = P.parens lexer
braces        = P.braces lexer
angles        = P.angles lexer
brackets      = P.brackets lexer
squares       = P.squares lexer
semi          = P.semi lexer
comma         = P.comma lexer
colon         = P.colon lexer
dot           = P.dot lexer
semiSep       = P.semiSep lexer
semiSep1      = P.semiSep1 lexer
commaSep      = P.commaSep lexer
commaSep1     = P.commaSep1 lexer
