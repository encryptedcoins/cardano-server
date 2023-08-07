#!/bin/bash

unset GTK_PATH

NODE_SOCK_PATH=../../../../encoins-tools/testnet-preprod/apps/cardano-node/node.sock
NODE_DB_PATH=../../../../encoins-tools/testnet-preprod/data/chain
KUPO_DB_PATH=../../../../encoins-tools/testnet-preprod/data
WALLET=../configuration/wallets/test-wallet.json

# Create the wallet file if it doesn't exist
if [ ! -f "$WALLET" ]; then
   cat "../configuration/wallets/test-wallet.json" > "$WALLET"
fi

# Run the scripts
gnome-terminal --tab --title="cardano-node" -- bash -c "./node.sh $NODE_DB_PATH $NODE_SOCK_PATH;$SHELL"
gnome-terminal --tab --title="cardano-wallet" -- bash -c "./wallet.sh $NODE_SOCK_PATH;$SHELL"
gnome-terminal --tab --title="kupo" -- bash -c "./kupo.sh $NODE_SOCK_PATH $KUPO_DB_PATH;$SHELL"
sh ./load_wallet.sh $WALLET