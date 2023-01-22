module Main where

import Tests.TestingServer (testFundsAllTS)
import Tests.Reference     (runReferenceTest)

main :: IO ()
main = runReferenceTest