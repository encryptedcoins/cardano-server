# Cardano-server

A lightweight backend server for hosting Cardano dApps. It is an alternative to Plutus Application Backend (PAB).

Key features:

1. Low resource consumption compared to PAB. Fast synchronization with other backend services (Cardano Node, Cardano Wallet Backend, Plutus Chain Index, Kupo (in the future)).

2. Fetching the actual blockchain data is handled automatically: smart contract developers can focus on the business logic of their apps.

3. Supports our transaction builder (check [this repo](https://github.com/encryptedcoins/plutus-apps-extra)).

4. A console client capable of sending correct requests to the server is created automatically (or with a few extra lines of code).

5. The client can emulate user behavior by sending randomized requests periodically, which might be helpful for stress-testing your app.

# How to use

To specialize cardano-server for your own application, make a new data type and define instances of the following classes: `HasServer`, `HasClient` (optional), and `HasTxEndpoints` (optional).

1. [HasServer](https://github.com/encryptedcoins/cardano-server/blob/main/src/Cardano/Server/Class.hs):

A class that defines a cardano-server:
* `AuxiliaryEnvOf` represents auxiliary environment data that the server must be aware of. Normally, this data contains parameters specific to your application.
* By default, the auxiliary environment data is loaded from a JSON file. You may change this behavior by redefining `loadAuxiliaryEnv` (optional).
* `InputOf` represents the type of external input the server may receive during execution.
* `serverSetup` defines actions performed during the server startup after all environment data is loaded.
* `serverIdle` defines actions that must be performed on repeat whenever the server is idle.
* `serverTrackedAddresses` defines actions that return the list of currently tracked Cardano network addresses. UTXOs from these addresses can be used for constructing transactions.

2. [HasClient](https://github.com/encryptedcoins/cardano-server/blob/main/src/Cardano/Server/Client/Class.hs):

A class that defines a console client corresponding to your cardano-server. The console client allows you to construct and send requests to the server using the command line.

* `parseServerInput` is the parser used to get `InputOf` your server from the command line. Optional if `InputOf` your server has a read instance.
* `genServerInput` is the generator used to generate `InputOf` your server. Optional if `InputOf` your server has a random instance. 
* `extractActionsFromInput` is a function that is used to obtain a pair of actions from the `InputOf` your server. These actions are performed before the request is sent and after a successful response is received, respectively. It can be useful, for example, if you need to write some additional information about your inputs to external files. Optional if you don't need to execute any actions.
* `addInputContext` is a function that will add `InputContext` to the `InputOf` your server before sending requests that require it. The default is an empty `ServerInputContext`.

Alternatively, you can use the [defaultClient](https://github.com/encryptedcoins/cardano-server/blob/main/src/Cardano/Server/Client/Default.hs) which requires only `FromJSON` instance of `InputOf` your server instead of the `HasClient` class. It reads an `InputOf` your server from the file and sends the respective request to the `sumbitTx` endpoint of your server.

3. [HasTxEndpoints](https://github.com/encryptedcoins/cardano-server/blob/main/src/Cardano/Server/Endpoints/Tx/Class.hs):

A class that defines three API endpoints on the server: `serverTx`, `newTx` and `sumbitTx`. This class defines the following:

* `TxApiRequestOf` is a type of request body that we expect to receive.
* `TxEndpointsErrorOf` is a type of errors that might be thrown while processing user requests to these endpoints.
* `txEndpointsProcessRequest` defines actions that convert `TxApiRequestOf s` into `(InputOf s, InputContext)` or throw a `TxEndpointsErrorOf s` error.
* `txEndpointsTxBuilders` defines actions that process the server input and return a list of transaction builders.

# Test server commands

This library includes the [test-server](https://github.com/encryptedcoins/cardano-server/blob/main/src/Cardano/Server/TestingServer/Main.hs) which is the simplest backend application that can be built on top of cardano-server. You can use it as a starting point for your own app development. Here is how to use the test-server and test-client.

1. Run server which works with test tokens:</br>
```console
$ cabal run testingServer
```

2. Run client in automatic mode in which it will send request to mint test tokens to selected endpoint (the default is SubmitTx) at an average *interval* seconds :</br>
```console
$ cabal run testingClient -- [Ping | ServerTx | NewTx | SubmitTx ] --auto -i interval
```
&emsp;&emsp;For example:
```console
$ cabal run testingClient -- SubmitTx --auto -i 30
```

3. Run client in manual mode in which it will send request to mint specified test *tokens* to selected endpoint (the default is SubmitTx):</br>
```console
$ cabal run testingClient -- [Ping | ServerTx | NewTx | SubmitTx ] --manual token token ...
```
&emsp;&emsp;For example:
```console
$ cabal run testingClient -- NewTx --manual a72kf wjr82ar4 ...
```