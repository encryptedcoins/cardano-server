module Main where

import Cardano.Server.Example.Main (runExampleServer)

main :: IO ()
main = runExampleServer "cardano-server-test/test/configuration/config.json"