# Cardano-server

A library with various utilities needed to deploy cardano-server.

Key features:

1. Low resource consumption compared to PAB. Fast synchronization with other backend services (Cardano Node, Cardano Wallet Backend, Plutus Chain Index, Kupo).

2. Fetching actual blockchain data is handled automatically: smart contract developers can focus on the business logic of their apps.

3. Support of our transaction construction DSL (check [this repo](https://github.com/encryptedcoins/plutus-apps-extra)).

4. A console client capable of sending correct requests to your server is created automatically.

5. The console client can emulate user behavior by sending randomized requests periodically, which might be helpful for stress-testing your app.

# How to use

This library comes with [one predefined instance for server and client] (https://github.com/encryptedcoins/cardano-server/blob/main/src/TestingServer/Main.hs). You can use it freely as a guide.

To use this library you need to define your own server data and make instances of
next classes for it. 

1. [HasServer] (https://github.com/encryptedcoins/cardano-server/blob/main/src/Server/Internal.hs):

This is a main cardano-server class where you need to define your server redeemer type (RedeemerOf), some auxiliary environment (AuxiliaryEnvOf) and how to get currency symbol (getCurrencySymbol). It also has some optional methods if you have any complex logic to get your environment (loadAuxiliaryEnv) or need to do something before starting server (setupServer). Besides, it has cycleTx method that you can use to make some transactions instead of deploying a whole server.

2. [HasClient] (https://github.com/encryptedcoins/cardano-server/blob/main/src/Client/Internal.hs):

A class for your client that parses list of tokens, builds redeemer from it and sends it to your server. Here you need to define type of parsing result of single token (RequestPieceOf), how to parse it (parseRequestPiece), how to gen it (genRequestPiece) and how to build redeemer (mkRedeemer). Note that in addition to redeemer mkRedeemer produce some action, that will be executed after successful response. You can use it for some file manipulations for example.

3. [HasTxEndpoints] (https://github.com/encryptedcoins/cardano-server/blob/main/src/Server/Endpoints/Tx/Internal.hs):

The last class for newTx and sumbitTx server endpoints. If you are using cycleTx and don't need to deploy a server you can freely skip it. Here you need to define data type with your custom errors (TxEndpointsErrorOf), error handler for it (txEndpointsErrorHanlder), error-checking function (checkForTxEndpointsErros) and function that will build list with constructors to make transactions in these endpoints (txEndpointsTxBuilders). In addition you need to define sum type with all possible results in these two endpoints (TxApiResultOf), but you can use DefaultTxApiResult if it fits your server. Also, here is an optional method that you can use to provide more addresses for your transactions if they need to interact with some external UTXO's.

# Test server commands

This library includes the test-server which is the simplest backend application that can be built on top of cardano-server. Here is how to use the test-server and test-client.

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