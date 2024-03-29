#!/bin/bash
cabal new-build all
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-server-example-1.0.0.0/x/cardano-server-example/build/cardano-server-example/cardano-server-example ~/.local/bin/cardano-server-example
cp dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-server-client-example-1.0.0.0/x/cardano-server-client-example/build/cardano-server-client-example/cardano-server-client-example ~/.local/bin/cardano-client-example