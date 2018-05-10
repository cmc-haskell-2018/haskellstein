module Main where

import System.Environment
import Haskellstein

main :: IO()
main = do
         windowInit
         args <- getArgs
         start args