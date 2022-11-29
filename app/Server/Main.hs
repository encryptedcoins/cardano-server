{-# LANGUAGE TypeApplications #-}

module Main where

import Server.Main        (runServer)
import TestingServer.Main (TestingServer)

main :: IO ()
main = runServer @TestingServer