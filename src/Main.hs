module Main where

import Control.Lens hiding ((<.>))
import Parser
import PrettyPrint
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.IO
import Text.PrettyPrint (render)


{-
This generated configuration will go between connected components in
  the original application. It will record the state transitions and
  will pass the commands and events. It is expected that
  this will not affect the callgraph of the original application.
-}

getConnections :: NescFile -> [Connection]
getConnections = toListOf (c . compImpl . traverse . cn)

generateConnectorConfiguration :: Connection -> (String, NescFile)
generateConnectorConfiguration x = (comp_name, C component)
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
    [user, provider, interface] = getNames x -- TODO: to get correct interface, we need to parse the module implementation
    newModule = interface ++ "ConnectorP"
    uInterface = interface ++ "U"
    pInterface = interface ++ "P"
    getNames (Equate    a d) = [head a, head d, lastElement a d]
    getNames (LeftLink  a d) = [head d, head a, lastElement a d]
    getNames (RightLink a d) = [head a, head d, lastElement a d]
    lastElement e1 e2 =
      last $ if (length e1 > length e2) then e1 else e2

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseNesc str >>= either print f
    _ -> error "please pass one argument with the file containing the text to parse"
  where
    f nescFile = do
      removeDirectoryRecursive distgen
      createDirectory distgen
      mapM_ (\(fname, content) ->
        writeFile (distgen </> fname <.> ".nc") (render $ prettyPrint content))
        (map generateConnectorConfiguration (getConnections nescFile))
      putStr $ "Files are generated in directory: " ++ distgen ++ "\n"
    distgen = "dist-gen"
