# Cardano-server

A lightweight backend server for hosting Cardano dApps. It is a robust alternative to Plutus Application Backend (PAB).

Key features:

1. Low resource consumption compared to PAB. Fast synchronization with other backend services (Cardano Node, Cardano Wallet Backend, Plutus Chain Index, Kupo).

2. Fetching actual blockchain data is handled automatically: smart contract developers can focus on the business logic of their apps.

3. Support of our transaction construction DSL (check [this repo](https://github.com/encryptedcoins/plutus-apps-extra)).

4. A console client capable of sending correct requests to your server is created automatically.

5. The console client can emulate user behavior by sending randomized requests periodically, which might be helpful for stress-testing your app.

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