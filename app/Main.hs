module Main where

import Lib

main :: IO ()
main = transformIngFileToHLedger "ing.csv"
