module ParserSpec where

import Test.Hspec
import Parser
import Text.Parsec.String
import System.Process
import Data.List (delete)

parse :: String -> IO ()
parse x = do
  loc <- readCreateProcess (shell $ "locate -l 1 /" ++ x ++ ".nc") ""
  parseFromFile myParser (delete '\n' loc) >>= f >>= (`shouldBe` True)
  where
    f (Left  _) = return False
    f (Right _) = return True

main :: IO ()
main = hspec $
  describe "Parse some interfaces" $ do
    it "Boot" $ parse "Boot"
    it "Timer" $ parse "Timer"
    it "Leds" $ parse "Leds"
    it "BlinkAppC" $ parse "BlinkAppC"
