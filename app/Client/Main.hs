{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Server.Client.Client      (startClient)
import Cardano.Server.TestingServer.Main (TestingServer)

main :: IO ()
main = startClient @TestingServer