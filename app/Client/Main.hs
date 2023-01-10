{-# LANGUAGE TypeApplications #-}

module Main where

import Client.Client      (startClient)
import TestingServer.Main (TestingServer)

main :: IO ()
main = startClient @TestingServer