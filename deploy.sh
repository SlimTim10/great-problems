#!/bin/bash
. stopserver.sh
nix-build -A exe --no-out-link
rm -rf dist
mkdir dist
ln -s $(nix-build -A exe --no-out-link)/* dist/
cp -r config dist
. startserver.sh
