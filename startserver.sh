#!/bin/sh
ADDRESS=$(hostname -I | awk '{print $1}')
(
	cd dist &&
		sudo ./backend \
			 --hostname=greatproblems.ca \
			 --address=$ADDRESS \
			 --port=80 \
			 --ssl-address=$ADDRESS \
			 --ssl-port=443 \
			 --ssl-cert=/etc/letsencrypt/live/greatproblems.ca/fullchain.pem \
			 --ssl-chain-cert \
			 --ssl-key=/etc/letsencrypt/live/greatproblems.ca/privkey.pem \
			 --access-log=/var/log/great-problems/access.log \
			 --error-log=/var/log/great-problems/error.log
			 >> /var/log/great-problems/output.log
)
