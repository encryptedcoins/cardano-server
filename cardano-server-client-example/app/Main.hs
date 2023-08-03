module Main where

import Cardano.Server.Client.Example.Main (runExampleClient)

main :: IO ()
main = runExampleClient "config.json"