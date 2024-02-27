module Main where

import App
import System.Environment qualified as ENV

main :: IO ()
main = do
  argList <- ENV.getArgs
  progName <- ENV.getProgName
  main' progName argList
