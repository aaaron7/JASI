module Main where

import Lib
import System.Environment
import Control.Monad
import Repl

main :: IO ()
main = do
  args <- getArgs
  if null args then repl else runOne $ args

-- main = do
--   line <- getChar
--   putStrLn [line]
