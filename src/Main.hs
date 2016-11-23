module Main where

import Control.Lens hiding ((<.>))
import Data.List (find, union)
import Parser
import PrettyPrint
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.PrettyPrint (render)

-- Hardcoded for now
getTosCPaths :: IO [(String, FilePath)]
getTosCPaths = do
  tosdir <- getEnv "TOSDIR"
  return [ ("MainC", tosdir </> "system/MainC.nc")
         , ("LedsC", tosdir </> "platforms/exp430/hardware/leds/LedsC.nc")
         , ("TimerMilliC", tosdir </> "system/TimerMilliC.nc")
         ]

getConnections :: NescFile -> [Connection]
getConnections = toListOf (c . compImpl . ci . traverse . cn)

getUsedComponents :: NescFile -> [Identifier]
getUsedComponents nescFile = comps `union` gen_comps
  where
    comps = toListOf (f . cri) nescFile
    gen_comps = toListOf (f . crnew) nescFile
    f = c . compImpl . ci . traverse . cc . traverse . compRef

{-
This generated configuration will go between connected components in
  the original application. It will record the state transitions and
  will pass the commands and events. It is expected that
  this will not affect the callgraph of the original application.
-}
generateConnectorConfiguration :: Connection -> (String, NescFile)
generateConnectorConfiguration x = (comp_name, C component)
  where
    component = Component GenericConfiguration comp_name (Just "") Nothing specification implementation
    comp_name = interface ++ "ConnectorC"
    specification =
      [Provides
        (SpecElem $ "interface " ++ interface)
      ]
    --           -- (SpecificationElement (InterfaceType interface Nothing) Nothing Nothing Nothing )
    implementation = CI
      [ CC [ComponentLine (CRNew newModule []) ""]
      , CC [ComponentLine (CRI provider) ""]
      , CN (RightLink (EndPoint [newModule, uInterface] Nothing) (EndPoint [provider, interface] Nothing))
      , CN (Equate (EndPoint [interface] Nothing) (EndPoint [newModule, pInterface] Nothing))
      ]
    [user, provider, interface] = getNames x -- TODO: to get correct interface, we need to parse the module implementation
    newModule = interface ++ "ConnectorP"
    uInterface = interface ++ "U"
    pInterface = interface ++ "P"
    getNames (Equate    a d) = [getHead a, getHead d, lastElement (getIdent a) (getIdent d)]
    getNames (LeftLink  a d) = [getHead d, getHead a, lastElement (getIdent a) (getIdent d)]
    getNames (RightLink a d) = [getHead a, getHead d, lastElement (getIdent a) (getIdent d)]
    lastElement e1 e2 =
      last $ if length e1 > length e2 then e1 else e2
    getHead (EndPoint xs _) = head xs
    getIdent (EndPoint xs _) = xs

getUsedInterfaces :: NescFile -> FilePath -> IO ()
getUsedInterfaces nescFile srcdir = do
  let usedComponents = getUsedComponents nescFile
  tosPaths <- getTosCPaths
  srcPathsAll <- getDirectoryContents srcdir
  let srcPaths = map (\e -> (dropExtension e, srcdir </> e)) $ filter (\x -> takeExtension x == ".nc") srcPathsAll
  let allPaths = filter (\(n,_) -> n `elem` usedComponents) (tosPaths `union` srcPaths)
  mapM_ (\(compName, compPath) -> getUsesInterfaces compPath) allPaths

getUsesInterfaces :: FilePath -> IO ()
getUsesInterfaces filePath = parseNesc filePath >>= either print f
  where
    f nescFile = print $ g nescFile
    g = toListOf (c . compSpec)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseNesc str >>= either print f --(g str)
    _ -> error "please pass one argument with the file containing the text to parse"
  where
    g srcpath nescFile = getUsedInterfaces nescFile (takeDirectory srcpath)
    f nescFile = do
      removeDirectoryRecursive distgen
      createDirectory distgen
      mapM_ (\(fname, content) ->
        writeFile (distgen </> fname <.> ".nc") (render $ prettyPrint content))
        (map generateConnectorConfiguration (getConnections nescFile))
      putStr $ "Files are generated in directory: " ++ distgen ++ "\n"
    distgen = "dist-gen"
