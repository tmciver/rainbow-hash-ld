#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

./result/bin/rhld-server --port 8081 --sparql-url http://localhost:3030/ds --file-store-url http://localhost:3001
