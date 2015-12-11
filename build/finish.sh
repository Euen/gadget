#!/bin/sh
cd /gadget/
exec 2>&1
exec pgrep -f 'name gadget@127.0.0.1' | xargs kill

