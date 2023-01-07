# Cardano-server

A lightweight backend server for hosting Cardano dApps. It is a robust alternative to Plutus Application Backend (PAB).

Key features:

1. Low resource consumption compared to PAB. Fast synchronization with other backend services (Cardano Node, Cardano Wallet Backend, Plutus Chain Index, Kupo).

2. Fetching actual blockchain data is handled automatically: smart contract developers can focus on the business logic of their apps.

3. Support of our transaction construction DSL (check [this repo](https://github.com/encryptedcoins/plutus-apps-extra)).

4. A console client capable of sending correct requests to your server is created automatically.

5. The console client can emulate user behavior by sending randomized requests periodically, which might be helpful for stress-testing your app.

# How to use

To use this library you need to make a new data type and define instances of (some of) the following classes.

1. [HasServer](https://github.com/encryptedcoins/cardano-server/blob/main/src/Server/Internal.hs):

This is the main class for defining a cardano-server. You need to define a redeemer type (`RedeemerOf`) for your application, auxiliary environment data that the server must be aware of (`AuxiliaryEnvOf`) and how to get currency symbol (`getCurrencySymbol`). Additionally, you may define the following functions: `loadAuxiliaryEnv` to get your environment data and `setupServer` to perform some actions during the server startup. You may use `cycleTx` function to define transactions that must be submitted to the network whenever possible.

2. [HasClient](https://github.com/encryptedcoins/cardano-server/blob/main/src/Client/Internal.hs):

A class for your client that parses list of tokens, builds redeemer from it and sends it to your server. Here you need to define a type for parsing a single request term (`RequestPieceOf`), how to parse it (`parseRequestPiece`), how to gen it (genRequestPiece) and how to build redeemer (mkRedeemer). Note that in addition to redeemer mkRedeemer produce some action, that will be executed after successful response. You can use it for some file manipulations for example.

3. [HasTxEndpoints](https://github.com/encryptedcoins/cardano-server/blob/main/src/Server/Endpoints/Tx/Internal.hs):

You can use this class to define server endpoints: `newTx` and `sumbitTx`. If you are using `cycleTx` and do not need to accept user requests, you do not need to define an instance of this class.

To make an instance of this class, define a data type with your own custom errors (`TxEndpointsErrorOf`), error handler for it (`txEndpointsErrorHanlder`), error-checking function (`checkForTxEndpointsErros`) and a monad action that returns a list of transaction builders for these endpoints (`txEndpointsTxBuilders`). In addition, you need to define sum type with all possible results in these two endpoints (TxApiResultOf), though you can use `DefaultTxApiResult` if it is suitable for your server. Also, there is an optional monad action that returns the list of addresses with UTXOs which may be used for constructing transactions.

# Test server commands

This library includes the [test-server](https://github.com/encryptedcoins/cardano-server/blob/main/src/TestingServer/Main.hs) which is the simplest backend application that can be built on top of cardano-server. You can use it as a starting point for your own app development. Here is how to use the test-server and test-client.

1. Run server which works with test tokens:</br>
```console
$ cabal run testingServer
```

2. Run client in automatic mode in which it will send up to *maximum* test tokens at an average *interval* seconds:</br>
```console
$ cabal run testingClient -- --auto -i interval -m maximum
```
&emsp;&emsp;For example:
```console
$ cabal run testingClient -- --auto -i 30 -m 5
```

3. Run client in manual mode in which it will mint specified test *tokens*:</br>
```console
$ cabal run testingClient -- --manual token token ...
```
&emsp;&emsp;For example:
```console
$ cabal run testingClient -- --manual a72kf wjr82ar4 ...
```
