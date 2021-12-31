#!/bin/sh
ps -ef | grep ./backend | grep -v grep | awk '{print $2}' | sudo xargs -r kill -9
