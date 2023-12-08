#!/bin/bash

cabal install --overwrite-policy=always && websocket-server --directory html
