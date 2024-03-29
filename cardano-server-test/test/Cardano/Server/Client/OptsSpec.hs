{-# LANGUAGE DataKinds                    #-}
{-# LANGUAGE KindSignatures               #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>"          #-}

module Cardano.Server.Client.OptsSpec where

import           Cardano.Server.Client.Example.Main (readInput)
import           Cardano.Server.Client.Internal     (Mode (Auto, Manual))
import           Cardano.Server.Client.Opts         (CommonOptions (..), runWithOpts)
import           Cardano.Server.Config              (ServerEndpoint (ServerTxE, SubmitTxE))
import qualified Data.Text                          as T
import           System.Environment                 (withArgs)
import           Test.Hspec                         (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Parsing client command line arguments" $ do

    it "auto" $ do

        withArgs ["submitTx","--auto","30"] runWithOpts >>=
            (`shouldBe` CommonOptions SubmitTxE (Auto 30))

    it "manual" $ do

        let input = "aaaa=2,aaaa=3"
            inputT = T.pack input

        withArgs ["serverTx","--manual", input] runWithOpts >>=
            (`shouldBe` CommonOptions ServerTxE (Manual inputT))

        readInput inputT >>= (`shouldBe` [("aaaa", 2),("aaaa", 3)])