module Main where

import TestingServer (testFundsAllTS)
import Reference     (runReferenceTest)

main :: IO ()
main = runReferenceTest