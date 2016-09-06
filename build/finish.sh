#!/bin/sh
cd /gadget/
exec 2>&1
exec pgrep -f 'name gadget_internal@172.4.0.10' | xargs kill

