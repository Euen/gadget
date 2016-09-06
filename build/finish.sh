#!/bin/sh
cd /gadget/
exec 2>&1
exec pgrep -f 'name gadget@172.4.0.11' | xargs kill

