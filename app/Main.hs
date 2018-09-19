module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    (f:year:_) <- getArgs
    parseAndEncode f year
