module Main where

import Lib

import Control.Applicative
import Options

data MainOptions = MainOptions

instance Options MainOptions where
    defineOptions = pure MainOptions

handleOptions :: MainOptions -> [String] -> IO ()
handleOptions _ = transformIngFilesToHLedger

main :: IO ()
main = runCommand handleOptions
