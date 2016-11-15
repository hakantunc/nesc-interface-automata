module Main where

import System.Environment
import Parser
import PrettyPrint
import Text.PrettyPrint (render)

class GetConnection a where
  getConnections :: a -> [Connection]

instance GetConnection NescFile where
  getConnections (I i) = []
  getConnections (C c) = getConnections c

instance GetConnection Component where
  getConnections (Component Configuration _ _ _ i) = getConnections i
  getConnections _ = []

instance (GetConnection a) => GetConnection [a] where
  getConnections [] = []
  getConnections xs = concatMap getConnections xs

instance GetConnection ConfigurationElement where
  getConnections (CN c) = [c]
  getConnections _ = []

{-
This generated configuration will go between connected components in
  the original application. It will record the state transitions and
  will pass the commands and events. It is expected that
  this will not affect the callgraph of the original application.
-} 
generateConnectorConfiguration :: Connection -> NescFile
generateConnectorConfiguration x = C component
  where
    component = Component GenericConfiguration comp_name (Just "") specification implementation
    comp_name = interface ++ "ConnectorC"
    specification = "provides interface " ++ interface
    implementation =
      [ CC [ComponentLine (CRNew newModule []) ""]
      , CC [ComponentLine (CRI provider) ""]
      , CN (RightLink [newModule, uInterface] [provider, interface])
      , CN (Equate [interface] [newModule, pInterface])
      ]
    [user, provider, interface] = getNames x
    newModule = interface ++ "ConnectorP"
    uInterface = interface ++ "U"
    pInterface = interface ++ "P"
    getNames (Equate a b)    = [head a, head b, lastElement a b]
    getNames (LeftLink a b) = [head b, head a, lastElement a b]
    getNames (RightLink a b)  = [head a, head b, lastElement a b]
    lastElement e1 e2 =
      last $ if (length e1 > length e2) then e1 else e2

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseNesc str >>= either print f
    _ -> error "please pass one argument with the file containing the text to parse"
  where
    f = putStr . render . prettyPrint . generateConnectorConfiguration . head . getConnections
