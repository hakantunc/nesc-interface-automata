module Main where

import Control.Lens hiding ((<.>))
import Data.List (union)
import qualified Data.Map.Strict as M
import Parser.Parser
import Parser.Syntax
import System.Directory
import System.Environment
import System.FilePath
import Text.Parsec

-- Hardcoded for now
getTosCPaths :: FilePath -> IO (M.Map Identifier FilePath)
getTosCPaths appPath = do
  let appDir = takeDirectory appPath
  srcPathsAll <- getDirectoryContents appDir
  let srcPaths = map (\e -> (dropExtension e, appDir </> e)) (filter (\x -> takeExtension x == ".nc") srcPathsAll)
  tosdir <- getEnv "TOSDIR"
  return $ M.fromList $
    [ ("MainC", tosdir </> "system/MainC.nc")
    , ("LedsC", tosdir </> "platforms/exp430/hardware/leds/LedsC.nc")
    , ("TimerMilliC", tosdir </> "system/TimerMilliC.nc")
    ] `union` srcPaths

getUsedComponents :: NescFile -> [Identifier]
getUsedComponents nescFile = comps `union` gen_comps
  where
    comps = toListOf (f . cri) nescFile
    gen_comps = toListOf (f . crnew) nescFile
    f = c . componentImpl . _Just . ci . traverse . cc . traverse . compRef

getUsesProvidesInterfaces :: NescFile -> ([Identifier], [Identifier])
getUsesProvidesInterfaces nescFile = f nescFile
  where
    f x = (toListOf (g . uses' . h) x, toListOf (g . provides . h) x)
    g = c . componentSpec . traverse
    h = specElem . specElemIType . interTypeIdent

parseListOfComps :: [(Identifier, FilePath)] -> IO [(Identifier, Either ParseError NescFile)]
parseListOfComps = mapM (\(i, f) -> sequence (i, parseNesc f))

generateConnectors :: NescFile -> FilePath -> IO ()
generateConnectors parsed appPath = do
  tosPaths <- getTosCPaths appPath
  let comps = mapM (getCompPaths tosPaths) $ getUsedComponents parsed
  case comps of
    Left err -> error err
    Right  c -> do
      compsParsed <- parseListOfComps c
      case mapM sequence compsParsed of
        Left err -> print err
        Right xs -> do
          let x = map (\(i, f) -> (i, getUsesProvidesInterfaces f)) xs
          print x
      return ()
  where
    getCompPaths tosPaths ind = case M.lookup ind tosPaths of
      Nothing -> Left $ "Missing component src: " ++ ind
      Just x  -> Right (ind, x)

main :: IO ()
main = do
  a <- getArgs
  case a of
    [path] -> do
      parsed <- parseNesc path
      case parsed of
        Left  x -> print x
        Right x -> generateConnectors x path
    _      -> error "please pass one argument"
