#!/bin/sh
ps -ef | grep dist/backend | grep -v grep | awk '{print $2}' | sudo xargs kill
