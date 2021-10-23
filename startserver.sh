#!/bin/sh
ADDRESS=$(hostname -I | awk '{print $1}')
sudo ./dist/backend --hostname=greatproblems.ca --address=$ADDRESS --port=80 --ssl-address=$ADDRESS --ssl-port=443 --ssl-cert=/etc/letsencrypt/live/greatproblems.ca/fullchain.pem --ssl-chain-cert --verbose --ssl-key=/etc/letsencrypt/live/greatproblems.ca/privkey.pem
