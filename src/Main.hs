module Main where

import System.Environment
import Parser

class GetConnection a where
  getConnections :: a -> [Connection]

instance GetConnection NescFile where
  getConnections (I i) = []
  getConnections (C c) = getConnections c

instance GetConnection Component where
  getConnections (Component Configuration _ _ i) = getConnections i
  getConnections _ = []

instance (GetConnection a) => GetConnection [a] where
  getConnections [] = []
  getConnections xs = concatMap getConnections xs

instance GetConnection ConfigurationElement where
  getConnections (CN c) = [c]
  getConnections _ = []

main :: IO ()
main = do
  a <- getArgs
  case a of
    [str] -> parseNesc str >>= either print (print . getConnections)
    _ -> error "please pass one argument with the file containing the text to parse"
