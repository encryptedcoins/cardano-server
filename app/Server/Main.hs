{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Server.Main               (runServer)
import Cardano.Server.TestingServer.Main (TestingServer)

main :: IO ()
main = runServer @TestingServer