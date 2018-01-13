module Main where

import Lib
import System.IO

main :: IO ()
main = do
    hSetEncoding stdout utf8
    someFunc
