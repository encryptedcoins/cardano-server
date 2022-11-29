# Cardano-server

A library with various utilities needed to deploying cardano-server.

# Usage

1. Run server which works with test tokens:</br>
cabal run testingServer

2. Run client in automatic mode in which it will send up to *maximum* test tokens at an average *interval*:</br>
cabal run testingClient -- --auto -i *interval* -m *maximum*

3. Run client in manual mode in which it will mint specified test *tokens*:</br>
cabal run testingClient -- --manual *token* *token* ...