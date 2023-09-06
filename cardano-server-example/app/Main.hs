module Main where

import Cardano.Server.Example.Main (runExampleServer)

main :: IO ()
main = runExampleServer "config.json"
