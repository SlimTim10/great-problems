#!/bin/bash
echo "Stopping server..."
. stopserver.sh

echo "Pulling repository..."
git checkout master
git pull origin master

echo "Building app..."
nix-build -A exe --no-out-link
rm -rf dist
mkdir dist
ln -s $(nix-build -A exe --no-out-link)/* dist/
cp -r config dist
echo "App built in dist/"

echo "Starting server..."
. startserver.sh
