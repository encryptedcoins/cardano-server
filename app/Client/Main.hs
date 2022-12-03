{-# LANGUAGE TypeApplications #-}

module Main where

import Client.Main        (startClient)
import TestingServer.Main (TestingServer)

main :: IO ()
main = startClient @TestingServer