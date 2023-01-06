# Cardano-server

A library with various utilities needed to deploy cardano-server.

# How to use

This library comes with one predefined instance for server and client. You can find it in TestingServer/Main and use freely as a guide.

To use this library you need to define your own server data and make instances of
next classes for it. 

1. HasServer from Server/Internal:

This is a main cardano-server class where you need to define your server redeemer type (RedeemerOf), some auxiliary environment (AuxiliaryEnvOf) and how to get currency symbol (getCurrencySymbol). It also has some optional methods if you have any complex logic to get your environment (loadAuxiliaryEnv) or need to do something before starting server (setupServer). Besides, it has cycleTx method that you can use to make some transactions instead of deploying a whole server.

2. HasClient from Client/Internal:

A class for your client that parses list of tokens, builds redeemer from it and sends it to your server. Here you need to define type of parsing result of single token (RequestPieceOf), how to parse it (parseRequestPiece), how to gen it (genRequestPiece) and how to build redeemer (mkRedeemer). Note that in addition to redeemer mkRedeemer produce some action, that will be executed after successful response. You can use it for some file manipulations for example.

3. HasTxEndpoints from Server/Endpoints/Tx/Internal:

The last class for newTx and sumbitTx server endpoints. If you are using cycleTx and don't need to deploy a server you can freely skip it. Here you need to define data type with your custom errors (TxEndpointsErrorOf), error handler for it (txEndpointsErrorHanlder), error-checking function (checkForTxEndpointsErros) and function that will build list with constructors to make transactions in these endpoints (txEndpointsTxBuilders). In addition you need to define sum type with all possible results in these two endpoints (TxApiResultOf), but you can use DefaultTxApiResult if it fits your server. Also, here is an optional method that you can use to provide more addresses for your transactions if they need to interact with some external UTXO's.

# Usage of testing server/client

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

