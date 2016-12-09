module ParserSpec where

import Control.Exception
import Parser.Parser
import Parser.Pretty
import System.Environment
import System.FilePath
import Test.Hspec
import Text.Parsec.String

parse :: String -> IO ()
parse loc = do
  tosdir <- getEnv "TOSDIR"
  parsed <- parseNesc $ tosdir </> loc
  -- either print print parsed
  -- either print (print . pp) parsed
  case parsed of
    Left err -> expectationFailure $ show err
    Right _  -> assert True (return ()) -- True `shouldBe` True

main :: IO ()
main = hspec $
  describe "Parser" $ do
    describe "Parse some interfaces" $ do
      it "Boot" $ parse "interfaces/Boot.nc"
      it "Leds" $ parse "interfaces/Leds.nc"
      it "Timer" $ parse "lib/timer/Timer.nc"
    describe "Parse some components" $ do
      it "BlinkAppC" $ parse "../apps/Blink/BlinkAppC.nc"
      it "BlinkC" $ parse "../apps/Blink/BlinkC.nc"
      it "MainC" $ parse "../tos/system/MainC.nc"
      it "LedsC" $ parse "../tos/system/LedsC.nc"
      it "LedsC" $ parse "../tos/system/TimerMilliC.nc"
      it "AMSenderC" $ parse "../tos/system/AMSenderC.nc"
