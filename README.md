# Cardano-server

A library with various utilities needed to deploy cardano-server.

# Usage

1. Run server which works with test tokens:</br>
```console
$ cabal run testingServer
```

2. Run client in automatic mode in which it will send up to *maximum* test tokens at an average *interval* seconds:</br>
```console
$ cabal run testingClient -- --auto -i *interval* -m *maximum*
```
&emsp;&emsp;For example:
```console
$ cabal run testingClient -- --auto -i 30 -m 5
```

3. Run client in manual mode in which it will mint specified test *tokens*:</br>
```console
$ cabal run testingClient -- --manual *token* *token* ...
```
&emsp;&emsp;For example:
```console
$ cabal run testingClient -- --manual a72kf wjr82ar4 ...
```